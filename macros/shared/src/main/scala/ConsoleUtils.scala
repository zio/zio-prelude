package zio.prelude

private[prelude] object ConsoleUtils {
  def underlined(s: String): String =
    Console.UNDERLINED + s + Console.RESET

  def green(s: String): String =
    Console.GREEN + s + Console.RESET

  def yellow(s: String): String =
    Console.YELLOW + s + Console.RESET

  def red(s: String): String =
    Console.RED + s + Console.RESET

  def blue(s: String): String =
    Console.BLUE + s + Console.RESET

  def magenta(s: String): String =
    Console.MAGENTA + s + Console.RESET

  def cyan(s: String): String =
    Console.CYAN + s + Console.RESET

  def dim(s: String): String =
    "\u001b[2m" + s + Console.RESET

  def bold(s: String): String =
    Console.BOLD + s + Console.RESET

}
