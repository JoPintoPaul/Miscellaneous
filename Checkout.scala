case class ShoppingList(items: List[Items]) {
  val totalPrice = items.map(_.totalPrice).sum
}

case class Items(item: Item, quantity: Int) {
  val totalPrice: Int = {
    item.specialOffer match {
      case Some(offer: SpecialOffer) =>
        val quantityOfSpecialOffers = quantity / offer.multiplier
        val quantityOfStandardPrice = quantity % offer.multiplier
        quantityOfSpecialOffers * offer.price + quantityOfStandardPrice * item.standardPrice
      case None => quantity * item.standardPrice
    }
  }
}

case class Item(sku: String, standardPrice: Int, specialOffer: Option[SpecialOffer] = None)

case class SpecialOffer(multiplier: Int, price: Int)

case class Transaction(prices: List[Item]) {
  def calculateTotal(itemsSkusAsString: String): Int = {
    val itemSkus: List[String] = itemsSkusAsString.split(",").toList
    val items: List[Item] = itemSkus flatMap (itemSku => prices find (price => price.sku.equals(itemSku)))
    val groupedItems: List[List[Item]] = items.groupBy(_.sku).values.toList
    val itemListMap: List[Items] = groupedItems.map(item => Items(item.head, item.length))
    val shoppingList = ShoppingList(itemListMap)
    shoppingList.totalPrice
  }
}
