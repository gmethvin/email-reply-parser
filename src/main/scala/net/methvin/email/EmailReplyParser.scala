package net.methvin.email

object EmailReplyParser {
  /**
   * Get the text of the visible portions of the email body
   *
   * @param text the String email body
   * @param includeSignatures a boolean indicating whether to include signatures
   * @return the parsed-out reply
   */
  def parseReply(text: String, includeSignatures: Boolean = false): String = {
    new EmailReplyParser(text).parsedText(includeSignatures = includeSignatures)
  }

  /**
   * Create a new email reply parser for an email body
   * @param text the email body
   * @return the new reply parser instance
   */
  def apply(text: String) = new EmailReplyParser(text)

  /**
   * A fragment of text in the email, with certain suspected properties
   */
  trait Part {
    def quoted: Boolean
    def signature: Boolean
    def hidden: Boolean
    def content: String
  }

  private val OnLine = """(?smi)(?!On.*On\s.+?wrote:)On\s.+?wrote:\s*$""".r
  private val QuoteHeader = """(?smi)^\s*:etorw.*?nO\s*$""".r
  private val Quoted = """(?smi)>\s*$""".r
  private val Signature = s"""(?smi)(--|__|\\w-)$$|(^\\s*\\.*(\\w+\\s*){1,5} ${"Sent from".reverse}$$)""".r
}

class EmailReplyParser(val text: String) {

  def parsedText: String = parsedText()

  def parsedText(includeSignatures: Boolean = false): String = {
    fragments.filter(f => !f.hidden || includeSignatures && f.signature).mkString("\n").trim
  }

  def parts: Seq[EmailReplyParser.Part] = fragments

  import EmailReplyParser._

  private var fragments: Seq[Fragment] = Seq()
  private var fragment: Option[Fragment] = None
  private var foundVisible: Boolean = false

  for (line <- OnLine.replaceAllIn(text.replace("\r\n", "\n"), _.group(0).replace("\n", " ")).reverse.lines) {
    val isQuoted = Quoted.findFirstIn(line).isDefined
    val isQuoteHeader = QuoteHeader.findFirstIn(line).isDefined
    val isEmpty = line.trim.isEmpty
    if (isEmpty) checkSignature()
    fragment = fragment collect {
      case f if f.quoted == isQuoted || f.quoted && (isEmpty || isQuoteHeader) =>
        f.copy(lines = f.lines :+ line)
    } orElse {
      finishFragment()
      Some(new Fragment(Seq(line), quoted = isQuoted))
    }
  }
  checkSignature()
  finishFragment()

  private def checkSignature(): Unit = {
    fragment = fragment map { f =>
      f.copy(signature = Signature.findFirstIn(f.lines.last).isDefined)
    }
    if (fragment.exists(_.signature)) finishFragment()
  }

  private def finishFragment(): Unit = {
    for (f <- fragment) {
      val hidden = !foundVisible && (f.quoted || f.signature || f.content.trim.isEmpty)
      foundVisible ||= !hidden
      fragments = f.copy(hidden = hidden) +: fragments
    }
    fragment = None
  }

  private case class Fragment(lines: Seq[String],
      quoted: Boolean = false, signature: Boolean = false, hidden: Boolean = false) extends Part {
    lazy val content = (lines mkString "\n").reverse
    override lazy val toString = content
  }
}
