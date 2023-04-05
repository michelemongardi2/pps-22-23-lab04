package u04lab.code

import List.*
import Option.*

trait ItemVA {
  def code: Int
  def name: String
  def tags: Seq[String]
}

object ItemVA:
  def apply(code: Int, name: String, tags: String*): ItemVA =
    ItemImplVA(code, name, tags)

  private case class ItemImplVA(override val code: Int,
                                override val name: String,
                                override val tags: Seq[String]) extends ItemVA

/**
 * A warehouse is a place where items are stored.
 */
trait WarehouseVA {
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: ItemVA): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tags: String): List[ItemVA]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Option[ItemVA]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: ItemVA): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
}

object WarehouseVA {
  def apply(): WarehouseVA = WarehouseImplVA()
  private case class WarehouseImplVA() extends WarehouseVA:

    private var itemsList: List[ItemVA] = empty

    override def store(item: ItemVA): Unit =
      itemsList = append(itemsList, cons(item, empty))
      println("Item added to the list: " + itemsList)

    override def contains(itemCode: Int): Boolean = find(itemsList)(_.code == itemCode) match
      case Some(_) => true
      case _ => false

    override def searchItems(tag: String): List[ItemVA] = filter(itemsList)(item => item.tags.contains(tag))
    //override def searchItems(tag: String): List[ItemVA] = filter(itemsList)(item => for i <- item.tags do if tag == i then true else false)

    override def retrieve(code: Int): Option[ItemVA] = find(itemsList)(_.code == code)

    override def remove(item: ItemVA): Unit =
      itemsList = filter(itemsList)(_ != item)
      println("Item " + item + " removed: " + itemsList)
}

@main def mainWarehouseVA(): Unit =
  val warehouse = WarehouseVA()

  val dellXps = ItemVA(33, "Dell XPS 15", "notebook")
  val dellInspiron = ItemVA(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = ItemVA(35, "Xiaomi S1", "moped", "mobility")


  println(warehouse.contains(dellXps.code)) // false
  println(warehouse.store(dellXps)) // side effect, add dell xps to the warehouse --> Cons(ItemImplVA(33,Dell XPS 15,ArraySeq(notebook)),Nil())
  println(warehouse.contains(dellXps.code)) // true
  println(warehouse.store(dellInspiron)) // side effect, add dell inspiron to the warehouse
  println(warehouse.store(xiaomiMoped)) // side effect, add xiaomi moped to the warehouse
  println(warehouse.searchItems("mobility")) // List(xiaomiMoped)
  println(warehouse.searchItems("notebook")) // List(dellXps, dellInspiron)
  println(warehouse.retrieve(11)) // None
  println(warehouse.retrieve(dellXps.code)) // Some(dellXps)
  println(warehouse.remove(dellXps)) // side effect, remove dell xps from the warehouse
  println(warehouse.retrieve(dellXps.code)) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/