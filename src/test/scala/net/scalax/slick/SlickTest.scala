package net.scalax.cpoi.test

import net.scalax.slick.SlickWrap
import org.scalatest._
import slick.jdbc.H2Profile.api.{ Tag => SlickTag, _ }
import io.circe.syntax._
import io.circe.generic.auto._

class SlickTest extends FlatSpec with Matchers with SlickWrap {

  case class Friend(id: Int, name: String, age: Int)

  class FriendTable(tag: SlickTag) extends Table[Friend](tag, "friend") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def age = column[Int]("age")

    def * = (id, name, age).mapTo[Friend]

    def orders = List(orderRep(id, "f_id"), orderRep(name, "f_name"), orderRep(age, "f_age"))
    def filters = List(filterWithKey(id, "f_id"), filterWithKey(name, "f_name"), filterWithKey(age, "f_age"))
  }

  object FriendTq extends TableQuery(cons => new FriendTable(cons))

  case class Mark(id: Int, name: String, mark: Int, friendId: Int)

  class MarkTable(tag: SlickTag) extends Table[Mark](tag, "mark") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def mark = column[Int]("mark")
    def friendId = column[Int]("friendId")

    def * = (id, name, mark, friendId).mapTo[Mark]

    def orders = List(orderRep(id, "m_id"), orderRep(name, "m_name"), orderRep(mark, "m_mark") /*,orderRep(friendId,"m_friendId")*/ )
    def filters = List(filterWithKey(id, "m_id"), filterWithKey(name, "m_name"), filterWithKey(mark, "m_mark") /*,orderRep(friendId,"m_friendId")*/ )

  }

  object MarkTq extends TableQuery(cons => new MarkTable(cons))

  "slick" should "sortBy dynamic" in {
    val query = for {
      friend <- FriendTq
      mark <- MarkTq if mark.friendId === friend.id
    } yield {
      (friend, mark)
    }
    val sortQuery1 = query.sortBy { case (friend, mark) => (friend.name.desc, mark.mark.asc) }
    val sortByFields2 = List(OrderPro("f_name", true), OrderPro("m_mark", false))
    val sortQuery2 = query.sortBy {
      case (friend, mark) =>
        val orders = friend.orders ::: mark.orders
        extractOrder(orders, sortByFields2)
    }
    println(sortQuery1.result.statements.toList)
    println(sortQuery2.result.statements.toList)
    (sortQuery1.result.statements.toList == sortQuery2.result.statements.toList) should be(true)

    val `一些額外或非法的輸入` = List(OrderPro("f_name", true), OrderPro("m_mark", false), OrderPro("m_friendId", false), OrderPro("nmoeantenrter", false))

    val sortQuery3 = query.sortBy {
      case (friend, mark) =>
        val orders = friend.orders ::: mark.orders
        extractOrder(orders, `一些額外或非法的輸入`)
    }
    println(sortQuery1.result.statements.toList)
    println(sortQuery3.result.statements.toList)
    (sortQuery1.result.statements.toList == sortQuery3.result.statements.toList) should be(true)
  }

  "slick" should "filter dynamic" in {
    val query = for {
      friend <- FriendTq
      mark <- MarkTq if mark.friendId === friend.id
    } yield {
      (friend, mark)
    }

    case class JsonFilterModel(f_name: String, m_mark: Int)
    case class JsonFilterModel2(f_name: String, m_mark: Int, m_friendId: Int, nmoeantenrter: String)

    val filterFields1 = JsonFilterModel("测试姓名1", 23)

    val filterQuery1 = query.filter { case (friend, mark) => (friend.name === filterFields1.f_name) && (mark.mark === filterFields1.m_mark) }
    val filterQuery2 = query.filter {
      case (friend, mark) =>
        val filters = friend.filters ::: mark.filters
        extractFilter(filterFields1.asJsonObject, filters)
    }
    println(filterQuery1.result.statements.toList)
    println(filterQuery2.result.statements.toList)
    (filterQuery1.result.statements.toList == filterQuery2.result.statements.toList) should be(true)

    val `一些額外或非法的輸入` = JsonFilterModel2("测试姓名1", 23, 4564, "sfw3rer")

    val filterQuery3 = query.filter {
      case (friend, mark) =>
        val filters = friend.filters ::: mark.filters
        extractFilter(`一些額外或非法的輸入`.asJsonObject, filters)
    }
    println(filterQuery1.result.statements.toList)
    println(filterQuery3.result.statements.toList)
    (filterQuery1.result.statements.toList == filterQuery3.result.statements.toList) should be(true)
  }

}