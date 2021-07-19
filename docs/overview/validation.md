---
id: overview_validation
title: "Validation"
---

In our normal ZIO programs, we simply `flatMap` our effects using a `for-comprehension`. If one of the effects returns an `Error`, the whole calculation stops at that point (unless we handle the Error).

But there are usecases where we don't want to stop our application, but rather collect all the errors. One example for a usecase like this is "form validation", another one might be validating input from other systems. 

# Example of form validation

```scala
final case class Email(address: String)
object Email {
    val regexp = "^(.+)@(.+)$".r
    def fromString(m: String): Option[Email] = m match {
        case regexp(e) => Some(e)
        case _ => None
    }
}

final case class User(name: String, age: Int, email: Email)
final case class ValidationError(msg: String)

import zio.prelude._

def validateName(u: User) = Validation.fromEither(
    Either.cond(u.name.length < 3, u.name, ValidationError("name too short"))
).orElse(Validation.fromEither(
    Either.cond(u.name.length < 30, u.name, ValidationError("name too long"))
)).map(_ => u.name) // orElse would normally generate a Tuple(u.name, u.name)

def validateAge(u: User) = Validation.fromEither(
    Either.cond(u.age > 13, u.age, ValidationError("too young"))
)

def validateEmail(u: User): Validation[ValidationError, Email] = Validation
    .fromOption(Email.fromString(u.email)) // this would return a Validation[Unit, Email]
    .mapError(_ => ValidationError("email is not valid")) // but we want a proper error type

// lets collect all the errors
val validations: Validation[ValidationError, User] = Validation.validate(
    validateName(user),
    validateAge(user),
    validateEmail(user)
)((name, age, email) => User(name, age, email))

val myEffect: ZIO[Any, Chunk[ValidationError], User] = for {
    _ <- ZIO.unit
    validated = validations.runLog
    (logs, errorsOrValue) = validated // you can ignore the logs for now
    user <- ZIO.fromEither(errorsOrValue)
} yield user

```

The code above should be straight-forward, but you might be wondering what is going on with the `logs` returned by `runLog` ? 

While other validation libraries only allow you to collect errors occured during validation, ZIO Prelude gives you another channel to 
collect information that e.g. might be helpful to debug your program. 

We can leverage this channel by using the `ZValidation` type, e.g. like this:

```scala

import zio.prelude._

def validateEmailAdvanced(u: User): ZValidation[String, ValidationError, Email] =  validateEmail(u).flatMap(_ => 
    if(u.email.endsWith("hotmail.com")) { 
        ZValidation.fail(ValidationError("email is not valid")).log("email is a blocked domain")
    } else { 
        ZValidation.succeed(u.email)
    }
)

val validations2: ZValidation[String, ValidationError, User] = Validation.validate(
    validateName(user),
    validateAge(user),
    validateEmailAdvanced(user)
)((name, age, email) => User(name, age, email))

val myEffect2: ZIO[ZEnv, Chunk[ValidationError], User] = for {
    _ <- ZIO.unit
    validated = validations2.runLog
    (logs, errorsOrValue) = validated
    _ <- ZIO.foreach_(logs){msg => ZIO.debug(msg)}
    user <- ZIO.fromEither(errorsOrValue)
} yield user

```

But you're not limited to String as a Logging Type. You can use anything you want, e.g. to properly utilize log levels of `zio-logging`:

```scala

sealed trait ValidationLog{
    def message: String
}

final case class ValidationLogInfo(message: String) extends ValidationLog
final case class ValidationLogError(message: String) extends ValidationLog


def validateEmailAdvanced2(u: User): ZValidation[ValidationLog, ValidationError, Email] =  validateEmail(u).flatMap(_ => 
    if(u.email.endsWith("hotmail.com")) { 
        ZValidation.fail(ValidationError("email is not valid")).log(ValidationLogDebug(s"email ${u.email} belongs to a blocked domain"))
    } else { 
        ZValidation.succeed(u.email)
    }
)

val validations3: ZValidation[ValidationLog, ValidationError, User] = Validation.validate(
    validateName(user),
    validateAge(user),
    validateEmailAdvanced2(user)
)((name, age, email) => User(name, age, email))


val myEffect3: ZIO[Has[Logging], Chunk[ValidationError], User] = for {
    _ <- ZIO.unit
    validated = validations3.runLog
    (logs, errorsOrValue) = validated
    _ <- ZIO.foreach_(logs){ 
        case ValidationLogInfo(m) => zio.logging.log.info(m)
        case ValidationLogDebug(m) => zio.logging.log.debug(m)
    }
    user <- ZIO.fromEither(errorsOrValue)
} yield user

```
