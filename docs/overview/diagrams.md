---
id: overview_diagrams
title: "Type Class diagrams"
---

# Equal

[![Equal hierarchy][Equal-image]][Equal-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  Equal~-A~ <|-- Hash~-A~
  Equal~-A~ <|-- Ord~-A~
  class Equal~A~{
    Chunk[A: Equal]
    Either[A: Equal, B: Equal]
    Exit[E: Equal, A: Equal]
    F[A: Equal]: DeriveEqual[_]
    List[A: Equal]
    Map[A, B: Equal]
    NonEmptyChunk[A: Equal]
    NonEmptyList[A: Equal]
    Option[A: Equal]
    These[A: Equal, B: Equal]
    Try[A: Equal]
    ❨T1: Equal, ..., T22: Equal❩
    Validation[E, A: Equal]
    Vector[A: Equal]
    ZNonEmptySet[A, B: Equal]
    ZSet[A, B: Hash]

    () both[B](=> Equal[B]): Equal[(A, B)]
    () bothWith[B, C](=> Equal[B])(C => (A, B)): Equal[C]
    () contramap[B](B => A): Equal[B]
    () either[B](=> Equal[B]): Equal[Either[A, B]]
    () eitherWith[B, C](=> Equal[B])(C => Either[A, B]): Equal[C]
    () equal(A, A): Boolean
    () notEqual(A, A): Boolean
    () toScala[A1 <: A]: scala.math.Equiv[A1]
  }
  class Hash~-A~ {
    Boolean
    Byte
    Cause[A]
    Char
    Chunk[A: Hash]
    Class[_]
    Double
    Either[A: Hash, B: Hash]
    F[A: Hash]: Derive[_, Hash]
    Fiber.Id
    Float
    Int
    List[A: Hash]
    Long
    Map[A, B: Hash]
    NonEmptyChunk[A: Hash]
    NonEmptyList[A: Hash]
    NonEmptySet[A]
    Nothing
    Option[A: Hash]
    Ordering
    Set[A]
    Short
    String
    These[A: Hash, B: Hash]
    Throwable
    ❨T1: Hash, ..., T22: Hash❩
    Unit
    Validation[E: Hash, A: Hash]
    Vector[A: Hash]
    ZNonEmptySet[A, B: Hash]
    ZSet[A, B: Hash]
    ZTrace

    () both[B](Hash[B]): Hash[(A, B)]
    () bothWith[B, C](Hash[B])(C => (A, B)): Hash[C]
    () contramap[B](B => A): Hash[B]
    () either[B](Hash[B]): Hash[Either[A, B]]
    () eitherWith[B, C](Hash[B])(C => Either[A, B]): Hash[C]
    () hash(A): Int
  }
  class Ord~-A~ {
    Boolean
    Byte
    Char
    Chunk[A: Ord]
    Double
    Either[A: Ord, B: Ord]
    F[A: Ord]: Derive[_, Ord]
    Fiber.Id
    Float
    Int
    List[A: Ord]
    Long
    NonEmptyChunk[A: Ord]
    NonEmptyList[A: Ord]
    Nothing
    Option[A: Ord]
    Ordering
    Short
    String
    ❨T1: Ord, ..., T22: Ord❩
    Unit
    Vector[A: Ord]

    () both[B](=> Ord[B]): Ord[(A, B)]
    () bothWith[B, C](=> Ord[B])(C => (A, B)): Ord[C]
    () compare(A, A): Ordering
    () contramap[B](B => A): Ord[B]
    () either[B](=> Ord[B]): Ord[Either[A, B]]
    () eitherWith[B, C](=> Ord[B])(C => Either[A, B]): Ord[C]
    () mapOrdering(Ordering => Ordering): Ord[A]
    () reverse: Ord[A]
    () toScala[A1 <: A]: scala.math.Ordering[A1]
  }
```
</details>

[Equal-image]: https://user-images.githubusercontent.com/9019485/95247858-b4c45300-0816-11eb-9e76-dd7cba0c7956.png
[Equal-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEVxdWFsfi1BfiA8fC0tIEhhc2h-LUF-XG4gIEVxdWFsfi1BfiA8fC0tIE9yZH4tQX5cbiAgY2xhc3MgRXF1YWx-QX57XG4gICAgQ2h1bmtbQTogRXF1YWxdXG4gICAgRWl0aGVyW0E6IEVxdWFsLCBCOiBFcXVhbF1cbiAgICBFeGl0W0U6IEVxdWFsLCBBOiBFcXVhbF1cbiAgICBGW0E6IEVxdWFsXTogRGVyaXZlRXF1YWxbX11cbiAgICBMaXN0W0E6IEVxdWFsXVxuICAgIE1hcFtBLCBCOiBFcXVhbF1cbiAgICBOb25FbXB0eUNodW5rW0E6IEVxdWFsXVxuICAgIE5vbkVtcHR5TGlzdFtBOiBFcXVhbF1cbiAgICBPcHRpb25bQTogRXF1YWxdXG4gICAgVHJ5W0E6IEVxdWFsXVxuICAgIOKfrkE6IEVxdWFsLCAuLi4sIFo6IEVxdWFs4p-vXG4gICAgVmFsaWRhdGlvbltFLCBBOiBFcXVhbF1cbiAgICBWZWN0b3JbQTogRXF1YWxdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IEVxdWFsXVxuICAgIFpTZXRbQSwgQjogSGFzaF1cblxuICAgICgpIGJvdGhbQl0oPT4gRXF1YWxbQl0pOiBFcXVhbFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oPT4gRXF1YWxbQl0pKEMgPT4gKEEsIEIpKTogRXF1YWxbQ11cbiAgICAoKSBjb250cmFtYXBbQl0oQiA9PiBBKTogRXF1YWxbQl1cbiAgICAoKSBlaXRoZXJbQl0oPT4gRXF1YWxbQl0pOiBFcXVhbFtFaXRoZXJbQSwgQl1dXG4gICAgKCkgZWl0aGVyV2l0aFtCLCBDXSg9PiBFcXVhbFtCXSkoQyA9PiBFaXRoZXJbQSwgQl0pOiBFcXVhbFtDXVxuICAgICgpIGVxdWFsKEEsIEEpOiBCb29sZWFuXG4gICAgKCkgbm90RXF1YWwoQSwgQSk6IEJvb2xlYW5cbiAgICAoKSB0b1NjYWxhW0ExIDw6IEFdOiBzY2FsYS5tYXRoLkVxdWl2W0ExXVxuICB9XG4gIGNsYXNzIEhhc2h-LUF-IHtcbiAgICBCb29sZWFuXG4gICAgQnl0ZVxuICAgIENhdXNlW0FdXG4gICAgQ2hhclxuICAgIENodW5rW0E6IEhhc2hdXG4gICAgQ2xhc3NbX11cbiAgICBEb3VibGVcbiAgICBFaXRoZXJbQTogSGFzaCwgQjogSGFzaF1cbiAgICBGW0E6IEhhc2hdOiBEZXJpdmVbXywgSGFzaF1cbiAgICBGaWJlci5JZFxuICAgIEZsb2F0XG4gICAgSW50XG4gICAgTGlzdFtBOiBIYXNoXVxuICAgIExvbmdcbiAgICBNYXBbQSwgQjogSGFzaF1cbiAgICBOb25FbXB0eUNodW5rW0E6IEhhc2hdXG4gICAgTm9uRW1wdHlMaXN0W0E6IEhhc2hdXG4gICAgTm9uRW1wdHlTZXRbQV1cbiAgICBOb3RoaW5nXG4gICAgT3B0aW9uW0E6IEhhc2hdXG4gICAgT3JkZXJpbmdcbiAgICBTZXRbQV1cbiAgICBTaG9ydFxuICAgIFN0cmluZ1xuICAgIFRocm93YWJsZVxuICAgIOKfrkE6IEhhc2gsIC4uLiwgWjogSGFzaOKfr1xuICAgIFVuaXRcbiAgICBWYWxpZGF0aW9uW0U6IEhhc2gsIEE6IEhhc2hdXG4gICAgVmVjdG9yW0E6IEhhc2hdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IEhhc2hdXG4gICAgWlNldFtBLCBCOiBIYXNoXVxuICAgIFpUcmFjZVxuXG4gICAgKCkgYm90aFtCXShIYXNoW0JdKTogSGFzaFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oSGFzaFtCXSkoQyA9PiAoQSwgQikpOiBIYXNoW0NdXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IEhhc2hbQl1cbiAgICAoKSBlaXRoZXJbQl0oSGFzaFtCXSk6IEhhc2hbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oSGFzaFtCXSkoQyA9PiBFaXRoZXJbQSwgQl0pOiBIYXNoW0NdXG4gICAgKCkgaGFzaChBKTogSW50XG4gIH1cbiAgY2xhc3MgT3Jkfi1BfiB7XG4gICAgQm9vbGVhblxuICAgIEJ5dGVcbiAgICBDaGFyXG4gICAgQ2h1bmtbQTogT3JkXVxuICAgIERvdWJsZVxuICAgIEVpdGhlcltBOiBPcmQsIEI6IE9yZF1cbiAgICBGW0E6IE9yZF06IERlcml2ZVtfLCBPcmRdXG4gICAgRmliZXIuSWRcbiAgICBGbG9hdFxuICAgIEludFxuICAgIExpc3RbQTogT3JkXVxuICAgIExvbmdcbiAgICBOb25FbXB0eUNodW5rW0E6IE9yZF1cbiAgICBOb25FbXB0eUxpc3RbQTogT3JkXVxuICAgIE5vdGhpbmdcbiAgICBPcHRpb25bQTogT3JkXVxuICAgIE9yZGVyaW5nXG4gICAgU2hvcnRcbiAgICBTdHJpbmdcbiAgICDin65BOiBPcmQsIC4uLiwgWjogT3Jk4p-vXG4gICAgVW5pdFxuICAgIFZlY3RvcltBOiBPcmRdXG5cbiAgICAoKSBib3RoW0JdKD0-IE9yZFtCXSk6IE9yZFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oPT4gT3JkW0JdKShDID0-IChBLCBCKSk6IE9yZFtDXVxuICAgICgpIGNvbXBhcmUoQSwgQSk6IE9yZGVyaW5nXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IE9yZFtCXVxuICAgICgpIGVpdGhlcltCXSg9PiBPcmRbQl0pOiBPcmRbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oPT4gT3JkW0JdKShDID0-IEVpdGhlcltBLCBCXSk6IE9yZFtDXVxuICAgICgpIG1hcE9yZGVyaW5nKE9yZGVyaW5nID0-IE9yZGVyaW5nKTogT3JkW0FdXG4gICAgKCkgcmV2ZXJzZTogT3JkW0FdXG4gICAgKCkgdG9TY2FsYVtBMSA8OiBBXTogc2NhbGEubWF0aC5PcmRlcmluZ1tBMV1cbiAgfVxuIiwibWVybWFpZCI6eyJ0aGVtZSI6ImRlZmF1bHQiLCJ0aGVtZVZhcmlhYmxlcyI6eyJiYWNrZ3JvdW5kIjoid2hpdGUiLCJwcmltYXJ5Q29sb3IiOiIjRUNFQ0ZGIiwic2Vjb25kYXJ5Q29sb3IiOiIjZmZmZmRlIiwidGVydGlhcnlDb2xvciI6ImhzbCg4MCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwicHJpbWFyeUJvcmRlckNvbG9yIjoiaHNsKDI0MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJzZWNvbmRhcnlCb3JkZXJDb2xvciI6ImhzbCg2MCwgNjAlLCA4My41Mjk0MTE3NjQ3JSkiLCJ0ZXJ0aWFyeUJvcmRlckNvbG9yIjoiaHNsKDgwLCA2MCUsIDg2LjI3NDUwOTgwMzklKSIsInByaW1hcnlUZXh0Q29sb3IiOiIjMTMxMzAwIiwic2Vjb25kYXJ5VGV4dENvbG9yIjoiIzAwMDAyMSIsInRlcnRpYXJ5VGV4dENvbG9yIjoicmdiKDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxLCA5LjUwMDAwMDAwMDEpIiwibGluZUNvbG9yIjoiIzMzMzMzMyIsInRleHRDb2xvciI6IiMzMzMiLCJtYWluQmtnIjoiI0VDRUNGRiIsInNlY29uZEJrZyI6IiNmZmZmZGUiLCJib3JkZXIxIjoiIzkzNzBEQiIsImJvcmRlcjIiOiIjYWFhYTMzIiwiYXJyb3doZWFkQ29sb3IiOiIjMzMzMzMzIiwiZm9udEZhbWlseSI6IlwidHJlYnVjaGV0IG1zXCIsIHZlcmRhbmEsIGFyaWFsIiwiZm9udFNpemUiOiIxNnB4IiwibGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsIm5vZGVCa2ciOiIjRUNFQ0ZGIiwibm9kZUJvcmRlciI6IiM5MzcwREIiLCJjbHVzdGVyQmtnIjoiI2ZmZmZkZSIsImNsdXN0ZXJCb3JkZXIiOiIjYWFhYTMzIiwiZGVmYXVsdExpbmtDb2xvciI6IiMzMzMzMzMiLCJ0aXRsZUNvbG9yIjoiIzMzMyIsImVkZ2VMYWJlbEJhY2tncm91bmQiOiIjZThlOGU4IiwiYWN0b3JCb3JkZXIiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJhY3RvckJrZyI6IiNFQ0VDRkYiLCJhY3RvclRleHRDb2xvciI6ImJsYWNrIiwiYWN0b3JMaW5lQ29sb3IiOiJncmV5Iiwic2lnbmFsQ29sb3IiOiIjMzMzIiwic2lnbmFsVGV4dENvbG9yIjoiIzMzMyIsImxhYmVsQm94QmtnQ29sb3IiOiIjRUNFQ0ZGIiwibGFiZWxCb3hCb3JkZXJDb2xvciI6ImhzbCgyNTkuNjI2MTY4MjI0MywgNTkuNzc2NTM2MzEyOCUsIDg3LjkwMTk2MDc4NDMlKSIsImxhYmVsVGV4dENvbG9yIjoiYmxhY2siLCJsb29wVGV4dENvbG9yIjoiYmxhY2siLCJub3RlQm9yZGVyQ29sb3IiOiIjYWFhYTMzIiwibm90ZUJrZ0NvbG9yIjoiI2ZmZjVhZCIsIm5vdGVUZXh0Q29sb3IiOiJibGFjayIsImFjdGl2YXRpb25Cb3JkZXJDb2xvciI6IiM2NjYiLCJhY3RpdmF0aW9uQmtnQ29sb3IiOiIjZjRmNGY0Iiwic2VxdWVuY2VOdW1iZXJDb2xvciI6IndoaXRlIiwic2VjdGlvbkJrZ0NvbG9yIjoicmdiYSgxMDIsIDEwMiwgMjU1LCAwLjQ5KSIsImFsdFNlY3Rpb25Ca2dDb2xvciI6IndoaXRlIiwic2VjdGlvbkJrZ0NvbG9yMiI6IiNmZmY0MDAiLCJ0YXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwidGFza0JrZ0NvbG9yIjoiIzhhOTBkZCIsInRhc2tUZXh0TGlnaHRDb2xvciI6IndoaXRlIiwidGFza1RleHRDb2xvciI6IndoaXRlIiwidGFza1RleHREYXJrQ29sb3IiOiJibGFjayIsInRhc2tUZXh0T3V0c2lkZUNvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dENsaWNrYWJsZUNvbG9yIjoiIzAwMzE2MyIsImFjdGl2ZVRhc2tCb3JkZXJDb2xvciI6IiM1MzRmYmMiLCJhY3RpdmVUYXNrQmtnQ29sb3IiOiIjYmZjN2ZmIiwiZ3JpZENvbG9yIjoibGlnaHRncmV5IiwiZG9uZVRhc2tCa2dDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQm9yZGVyQ29sb3IiOiJncmV5IiwiY3JpdEJvcmRlckNvbG9yIjoiI2ZmODg4OCIsImNyaXRCa2dDb2xvciI6InJlZCIsInRvZGF5TGluZUNvbG9yIjoicmVkIiwibGFiZWxDb2xvciI6ImJsYWNrIiwiZXJyb3JCa2dDb2xvciI6IiM1NTIyMjIiLCJlcnJvclRleHRDb2xvciI6IiM1NTIyMjIiLCJjbGFzc1RleHQiOiIjMTMxMzAwIiwiZmlsbFR5cGUwIjoiI0VDRUNGRiIsImZpbGxUeXBlMSI6IiNmZmZmZGUiLCJmaWxsVHlwZTIiOiJoc2woMzA0LCAxMDAlLCA5Ni4yNzQ1MDk4MDM5JSkiLCJmaWxsVHlwZTMiOiJoc2woMTI0LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTQiOiJoc2woMTc2LCAxMDAlLCA5Ni4yNzQ1MDk4MDM5JSkiLCJmaWxsVHlwZTUiOiJoc2woLTQsIDEwMCUsIDkzLjUyOTQxMTc2NDclKSIsImZpbGxUeXBlNiI6ImhzbCg4LCAxMDAlLCA5Ni4yNzQ1MDk4MDM5JSkiLCJmaWxsVHlwZTciOiJoc2woMTg4LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkifX0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9


# Associative

[![Associative hierarchy][Associative-image]][Associative-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  Associative~A~ <|-- Commutative~A~
  Associative~A~ <|-- Idempotent~A~
  Associative~A~ <|-- Identity~A~
  Identity~A~ <|-- Inverse~A~
  class Associative~A~{
    Either[E, A: Associative]
    F[A: Associative]: Derive[_, Associative]
    First[A]
    Last[A]
    NonEmptyChunk[A]
    NonEmptyList[A]
    These[A: Associative, B: Associative]
    ❨T1: Associative, ..., T22: Associative❩
    Validation[E, A: Associative]
    ZNonEmptySet[A, B: Associative]

    () combine(=> A, => A): A
  }
  class Commutative~A~{
    And
    F[A: Commutative]: Derive[_, Commutative]
    Either[E: Commutative, A: Commutative]
    Or
    Map[K, V: Commutative]
    Max[A: Ord]
    Max[Boolean]
    Max[Byte/Char/Double/Float/Int/Long/Short]
    Min[A: Ord]
    Min[Boolean]
    Min[Byte/Char/Double/Float/Int/Long/Short]
    NonEmptySet[A]
    Option[A: Commutative]
    Prod[Boolean]
    Prod[Byte/Char/Double/Float/Int/Long/Short]
    Set[A]
    Sum[Boolean]
    Sum[Byte/Char/Double/Float/Int/Long/Short]
    These[A: Commutative❩, B: Commutative❩]
    ❨T1: Commutative, ..., T22: Commutative❩
    Validation[E, A: Commutative]
    ZSet[A, B: Commutative]
    ZNonEmptySet[A, B: Commutative]

    () commute: Commutative[A]
  }
  class Idempotent~A~{
    And
    F[A: Idempotent]: Derive[_, Idempotent]
    Or
    Map[K, V: Idempotent]
    Max[Boolean]
    Max[Byte/Char/Double/Float/Int/Long/Short]
    Min[Boolean]
    Min[Byte/Char/Double/Float/Int/Long/Short]
    NonEmptySet[A]
    Option[A: Idempotent]
    Prod[Boolean]
    Set[A]
    Sum[Boolean]
    These[A: Idempotent❩, B: Idempotent❩]
    ❨T1: Idempotent, ..., T22: Idempotent❩
    Validation[E, A: Idempotent]
    ZSet[A, B: Idempotent]
    ZNonEmptySet[A, B: Idempotent]

    () combineIdempotent(=> A, => A)(Equal[A]): A
    () idempotent(Equal[A]): Idempotent[A]
  }
  class Identity~A~{
    F[A: Identity]: Derive[_, Identity]
    Chunk[A]
    Either[E, A: Identity]
    List[A]
    Map[K, V: Associative]
    Max[Boolean]
    Max[Byte/Char/Double/Float/Int/Long/Short]
    Min[Boolean]
    Min[Byte/Char/Double/Float/Int/Long/Short]
    Option[A: Associative]
    Prod[Byte/Char/Double/Float/Int/Long/Short]
    String
    ❨T1: Identity, ..., T22: Identity❩
    Validation[E, A: Identity]
    Vector[A]
    ZSet[A, B: Associative]

    () identity: A
  }
  class Inverse~A~{
    And
    F[A: Inverse]: Derive[_, Inverse]
    Or
    Prod[Boolean]
    Set[A]
    Sum[Boolean]
    Sum[Byte/Char/Double/Float/Int/Long/Short]
    ❨T1: Inverse, ..., T22: Inverse❩

    () inverse(=> A, => A): A
  }
```

</details>

[Associative-image]: https://user-images.githubusercontent.com/9019485/95663570-90cb7f00-0b40-11eb-8886-b23a7e71cc72.png
[Associative-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFzc29jaWF0aXZlfkF-IDx8LS0gQ29tbXV0YXRpdmV-QX5cbiAgQXNzb2NpYXRpdmV-QX4gPHwtLSBJZGVtcG90ZW50fkF-XG4gIEFzc29jaWF0aXZlfkF-IDx8LS0gSWRlbnRpdHl-QX5cbiAgSWRlbnRpdHl-QX4gPHwtLSBJbnZlcnNlfkF-XG4gIGNsYXNzIEFzc29jaWF0aXZlfkF-e1xuICAgIEVpdGhlcltFLCBBOiBBc3NvY2lhdGl2ZV1cbiAgICBGW0E6IEFzc29jaWF0aXZlXTogRGVyaXZlW18sIEFzc29jaWF0aXZlXVxuICAgIEZpcnN0W0FdXG4gICAgTGFzdFtBXVxuICAgIE5vbkVtcHR5Q2h1bmtbQV1cbiAgICBOb25FbXB0eUxpc3RbQV1cbiAgICBUaGVzZVtBOiBBc3NvY2lhdGl2ZSwgQjogQXNzb2NpYXRpdmVdXG4gICAg4p2oVDE6IEFzc29jaWF0aXZlLCAuLi4sIFQyMjogQXNzb2NpYXRpdmXinalcbiAgICBWYWxpZGF0aW9uW0UsIEE6IEFzc29jaWF0aXZlXVxuICAgIFpOb25FbXB0eVNldFtBLCBCOiBBc3NvY2lhdGl2ZV1cblxuICAgICgpIGNvbWJpbmUoPT4gQSwgPT4gQSk6IEFcbiAgfVxuICBjbGFzcyBDb21tdXRhdGl2ZX5BfntcbiAgICBBbmRcbiAgICBGW0E6IENvbW11dGF0aXZlXTogRGVyaXZlW18sIENvbW11dGF0aXZlXVxuICAgIEVpdGhlcltFOiBDb21tdXRhdGl2ZSwgQTogQ29tbXV0YXRpdmVdXG4gICAgT3JcbiAgICBNYXBbSywgVjogQ29tbXV0YXRpdmVdXG4gICAgTWF4W0E6IE9yZF1cbiAgICBNYXhbQm9vbGVhbl1cbiAgICBNYXhbQnl0ZS9DaGFyL0RvdWJsZS9GbG9hdC9JbnQvTG9uZy9TaG9ydF1cbiAgICBNaW5bQTogT3JkXVxuICAgIE1pbltCb29sZWFuXVxuICAgIE1pbltCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE5vbkVtcHR5U2V0W0FdXG4gICAgT3B0aW9uW0E6IENvbW11dGF0aXZlXVxuICAgIFByb2RbQm9vbGVhbl1cbiAgICBQcm9kW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgU2V0W0FdXG4gICAgU3VtW0Jvb2xlYW5dXG4gICAgU3VtW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgVGhlc2VbQTogQ29tbXV0YXRpdmXinaksIEI6IENvbW11dGF0aXZl4p2pXVxuICAgIOKdqFQxOiBDb21tdXRhdGl2ZSwgLi4uLCBUMjI6IENvbW11dGF0aXZl4p2pXG4gICAgVmFsaWRhdGlvbltFLCBBOiBDb21tdXRhdGl2ZV1cbiAgICBaU2V0W0EsIEI6IENvbW11dGF0aXZlXVxuICAgIFpOb25FbXB0eVNldFtBLCBCOiBDb21tdXRhdGl2ZV1cblxuICAgICgpIGNvbW11dGU6IENvbW11dGF0aXZlW0FdXG4gIH1cbiAgY2xhc3MgSWRlbXBvdGVudH5BfntcbiAgICBBbmRcbiAgICBGW0E6IElkZW1wb3RlbnRdOiBEZXJpdmVbXywgSWRlbXBvdGVudF1cbiAgICBPclxuICAgIE1hcFtLLCBWOiBJZGVtcG90ZW50XVxuICAgIE1heFtCb29sZWFuXVxuICAgIE1heFtCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE1pbltCb29sZWFuXVxuICAgIE1pbltCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE5vbkVtcHR5U2V0W0FdXG4gICAgT3B0aW9uW0E6IElkZW1wb3RlbnRdXG4gICAgUHJvZFtCb29sZWFuXVxuICAgIFNldFtBXVxuICAgIFN1bVtCb29sZWFuXVxuICAgIFRoZXNlW0E6IElkZW1wb3RlbnTinaksIEI6IElkZW1wb3RlbnTinaldXG4gICAg4p2oVDE6IElkZW1wb3RlbnQsIC4uLiwgVDIyOiBJZGVtcG90ZW504p2pXG4gICAgVmFsaWRhdGlvbltFLCBBOiBJZGVtcG90ZW50XVxuICAgIFpTZXRbQSwgQjogSWRlbXBvdGVudF1cbiAgICBaTm9uRW1wdHlTZXRbQSwgQjogSWRlbXBvdGVudF1cblxuICAgICgpIGNvbWJpbmVJZGVtcG90ZW50KD0-IEEsID0-IEEpKEVxdWFsW0FdKTogQVxuICAgICgpIGlkZW1wb3RlbnQoRXF1YWxbQV0pOiBJZGVtcG90ZW50W0FdXG4gIH1cbiAgY2xhc3MgSWRlbnRpdHl-QX57XG4gICAgRltBOiBJZGVudGl0eV06IERlcml2ZVtfLCBJZGVudGl0eV1cbiAgICBDaHVua1tBXVxuICAgIEVpdGhlcltFLCBBOiBJZGVudGl0eV1cbiAgICBMaXN0W0FdXG4gICAgTWFwW0ssIFY6IEFzc29jaWF0aXZlXVxuICAgIE1heFtCb29sZWFuXVxuICAgIE1heFtCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE1pbltCb29sZWFuXVxuICAgIE1pbltCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE9wdGlvbltBOiBBc3NvY2lhdGl2ZV1cbiAgICBQcm9kW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgU3RyaW5nXG4gICAg4p2oVDE6IElkZW50aXR5LCAuLi4sIFQyMjogSWRlbnRpdHninalcbiAgICBWYWxpZGF0aW9uW0UsIEE6IElkZW50aXR5XVxuICAgIFZlY3RvcltBXVxuICAgIFpTZXRbQSwgQjogQXNzb2NpYXRpdmVdXG5cbiAgICAoKSBpZGVudGl0eTogQVxuICB9XG4gIGNsYXNzIEludmVyc2V-QX57XG4gICAgQW5kXG4gICAgRltBOiBJbnZlcnNlXTogRGVyaXZlW18sIEludmVyc2VdXG4gICAgT3JcbiAgICBQcm9kW0Jvb2xlYW5dXG4gICAgU2V0W0FdXG4gICAgU3VtW0Jvb2xlYW5dXG4gICAgU3VtW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAg4p2oVDE6IEludmVyc2UsIC4uLiwgVDIyOiBJbnZlcnNl4p2pXG5cbiAgICAoKSBpbnZlcnNlKD0-IEEsID0-IEEpOiBBXG4gIH1cbiIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ
