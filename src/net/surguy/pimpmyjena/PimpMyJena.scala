package net.surguy.pimpmyjena


import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.shared._
import com.hp.hpl.jena.util.iterator._
import com.hp.hpl.jena.ontology._

import scala.collection._
import scala.collection.mutable.ArrayBuffer

object Conversions {
  // How can I have this passed in to the object? An implicit defined elsewhere?
  val baseUri = "http://surguy.net/olympics/"

//  implicit def pimpExtendedIterator(o: ExtendedIterator) = new RichExtendedIterator(o)
  implicit def pimpRDFNode(o: RDFNode) = new RichRDFNode(o)
  implicit def pimpIndividual(o: Individual) = new RichIndividual(o,baseUri)
  implicit def pimpOntModel(o : OntModel) = new RichOntModel(o, baseUri)
}

//class RichExtendedIterator[T](it:ExtendedIterator) extends Iterable[T] {
//    def elements = delegateIterator().elements[T]
//    private def delegateIterator() : scala.Iterator[T] = new Iterator[T] {
//      def hasNext = it.hasNext
//      def next = it.next
//    }
//}

class RichOntModel(o:OntModel, defaultNamespace : String) {
  // @todo This is a rather crude way of converting the ExtendedIterator into a Scala iterator
  def individuals(r: Resource) = list[Individual](o.listIndividuals(r))
  def individuals(s: String) = list[Individual](o.listIndividuals(resource(s)))

  private def resource(r:String) = {
    if (r.contains(":"))
      ResourceFactory.createProperty(r) else
      ResourceFactory.createProperty(defaultNamespace, r)
  }

  private def list[T](it:ExtendedIterator) = {
     var l = new ArrayBuffer[T]();
     while (it.hasNext) l += it.next.asInstanceOf[T]
     l
   }

}


class RichRDFNode(r:RDFNode) {
  def stringValue = if (r.isLiteral) r.as(classOf[Literal]).asInstanceOf[Literal].getString else r.toString
}

class RichIndividual(i:Individual, defaultNamespace:String) {
  def apply(predicate:String) =  i.getPropertyValue(property(predicate))
  def update(predicate:String, value:String) = i.setPropertyValue( property(predicate), literal(value) )
  def update(predicate:String, value:RDFNode) = i.setPropertyValue( property(predicate), value )

  def addLabel(label : String) = i.addLabel(literal(label))
  def addLabel(label : RDFNode) = i.addLabel( literal(label.toString) )

  private def property(p:String) = {
    if (p.contains(":"))
      ResourceFactory.createProperty(p) else
      ResourceFactory.createProperty(defaultNamespace, p)
  }
  private def literal(value:String) = ResourceFactory.createPlainLiteral(value)
}

