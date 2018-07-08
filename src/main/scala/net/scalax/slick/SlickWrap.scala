package net.scalax.slick

import io.circe.{ Decoder, Json, JsonObject }
import slick.ast.BaseTypedType

trait SlickWrap {

  /*
  sortBy
   */

  trait OrderByWrap {
    val key: String
    type Rep
    val rep: Rep
    val orderByGen: Rep => slick.lifted.Ordered
  }

  def orderRep[T](column: T, key: String)(implicit cv: T => slick.lifted.Ordered): OrderByWrap = {
    val key1 = key
    new OrderByWrap {
      override val key = key1
      type Rep = T
      val rep = column
      override val orderByGen = cv
    }
  }

  case class OrderPro(key: String, isDesc: Boolean)
  def extractOrder(orderWraps: List[OrderByWrap], names: List[OrderPro]): slick.lifted.Ordered = {
    val ords = names
      .map(name => (name.isDesc, orderWraps.find(r => name.key == r.key)))
      .collect { case (isDesc, Some(wrap)) => (wrap, isDesc) }
      .map {
        case (wrap, isDesc) =>
          val orders = wrap.orderByGen(wrap.rep)
          if (isDesc) {
            new slick.lifted.Ordered(orders.columns.map { case (node, ord) => (node, ord.desc) })
          } else {
            new slick.lifted.Ordered(orders.columns.map { case (node, ord) => (node, ord.asc) })
          }
      }

    ords match {
      case Nil =>
        new slick.lifted.Ordered(Vector.empty)
      case list @ (_ :: _) =>
        list.reduce((s, t) => new slick.lifted.Ordered(s.columns ++: t.columns))
    }
  }

  /*
  fitler
   */
  trait FilterWrap[RepType] {

    type DataType
    val decoder: Decoder[DataType]
    type BaseType
    val cv: DataType => Option[BaseType]
    val bt: BaseTypedType[BaseType]
    //val rep: RepType
    val repCv: RepType => slick.lifted.Rep[Option[BaseType]]
    val profile: slick.jdbc.JdbcProfile

    def fromJson(json: JsonObject, key: String, rep: RepType): Option[slick.lifted.Rep[Option[Boolean]]] = {
      val data = json(key).flatMap(j => j.as[DataType](decoder).toOption)

      import profile.api._
      implicit val btImplicit = bt

      data match {
        case Some(d) =>
          Option(repCv(rep) === cv(d))
        case _ =>
          Option.empty
      }
    }
  }

  object FilterWrap {
    implicit def nonOptionImplicit[T](implicit bt: BaseTypedType[T], profile: slick.jdbc.JdbcProfile, decoder: Decoder[T]): FilterWrap[slick.lifted.Rep[T]] = {
      val decoder1 = decoder
      import profile.api._
      val bt1 = bt
      val profile1 = profile
      new FilterWrap[slick.lifted.Rep[T]] {
        override type DataType = T
        override val decoder = decoder1
        override type BaseType = T
        override val cv = (base: T) => Option(base)
        override implicit val bt: BaseTypedType[BaseType] = bt1
        override val repCv = (rep: slick.lifted.Rep[T]) => rep.?
        override val profile = profile1
      }
    }

    implicit def optionImplicit[T](implicit bt: BaseTypedType[T], profile: slick.jdbc.JdbcProfile, decoder: Decoder[Option[T]]): FilterWrap[slick.lifted.Rep[Option[T]]] = {
      val decoder1 = decoder
      val bt1 = bt
      val profile1 = profile
      new FilterWrap[slick.lifted.Rep[Option[T]]] {
        override type DataType = Option[T]
        override val decoder = decoder1
        override type BaseType = T
        override val cv = (base: DataType) => base
        override implicit val bt: BaseTypedType[BaseType] = bt1
        override val repCv = (rep: slick.lifted.Rep[Option[T]]) => rep
        override val profile = profile1
      }
    }
  }

  trait FilterRep {
    val key: String
    type RepType
    val rep: RepType
    val wrap: FilterWrap[RepType]
    def filterFromJson(json: JsonObject): Option[slick.lifted.Rep[Option[Boolean]]] = {
      wrap.fromJson(json, key, rep)
    }
  }

  def filterWithKey[Rep](rep: Rep, key: String)(implicit filterRep: FilterWrap[Rep]): FilterRep = {
    val key1 = key
    val rep1 = rep
    new FilterRep {
      override val key = key1
      override type RepType = Rep
      override val rep = rep1
      override val wrap = filterRep
    }
  }

  def extractFilter(jobj: JsonObject, filters: List[FilterRep])(implicit profile: slick.jdbc.JdbcProfile): slick.lifted.Rep[Option[Boolean]] = {
    import profile.api._
    filters match {
      case Nil => slick.lifted.LiteralColumn(Option(true))
      case list @ (head :: _) =>
        list.map(item => item.filterFromJson(jobj)).collect { case Some(s) => s }.reduce(_ && _)
    }
  }

}