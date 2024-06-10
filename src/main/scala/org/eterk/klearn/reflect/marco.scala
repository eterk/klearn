package org.eter.klearn
package reflect

object marco {

  //

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  def fieldsNameMacro[T]: Seq[String] = macro fieldsNameMacroImpl[T]

  def fieldsNameMacroImpl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val fieldNames = tpe.decls.collect {
      case m: MethodSymbol if m.isCaseAccessor => m.name.toString
    }.toSeq.reverse

    c.Expr[Seq[String]](q"Seq(..$fieldNames)")

  }






}


