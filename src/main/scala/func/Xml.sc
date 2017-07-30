
val elem =
  <a>
    Compiler is in XML mode
    <atata/>
    <azaza/>
  </a>
println("Scala mode is back")

case class CCTherm(description: String,
                   yearMade: Int,
                   dateObtained: String,
                   bookPrice: Int,
                   purchasePrice: Int,
                   condition: Int) {
  override def toString: String = description

  def toXML =
    <ccterm>
      <description>{description}</description>
      <yearMade>{yearMade}</yearMade>
      <dateObtained>{dateObtained}</dateObtained>
      <bookPrice>{bookPrice}</bookPrice>
      <purchasePrice>{purchasePrice}</purchasePrice>
      <condition>{condition}</condition>
    </ccterm>
}

/*
def fromXml(node: scala.xml.Node): CCTherm = CCTherm(
  (node \ "description").text,
  (node \ "yearMade").text,
  (node \ "dateObtained").text,
  (node \ "bookPrice").text,
  (node \ "purchasePrice").text,
  (node \ "condition").text,
)
*/

val therm = CCTherm(
  description = "hot dog #5",
  yearMade = 1952,
  dateObtained = "March 14, 2006",
  bookPrice = 2199,
  purchasePrice = 500,
  condition = 9
)

val xmlTherm = therm.toXML
//val therm1 = fromXml(xmlTherm)

<a>
  Feels
  <b>good</b>
  man!
</a>.text

val sasXml =
  <sas>
    <sos>
      <kek a="zoo">0</kek>
      <kek a="zoz">1</kek>
      <kek a="mam">10</kek>
    </sos>
    <sos>
      <kek>3</kek>
      <kek>2</kek>
      <kek>1</kek>
    </sos>
  </sas>
sasXml \ "sos"
sasXml \\ "kek"
sasXml \\ "@a"


