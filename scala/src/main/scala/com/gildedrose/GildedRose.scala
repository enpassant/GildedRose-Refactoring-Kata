package com.gildedrose

class GildedRose(val items: List[Item]) {
  import GildedRose._

  def updateQuality(): List[Item] = {
    items map {
      updateAged orElse
      updateBackstage orElse
      updateSulfuras orElse
      updateConjured orElse
      updateNormal
    }
  }

  private val updateAged: PartialFunction[Item, Item] = {
    case item @ Item(TYPE_AGED, sellIn, quality) =>
      val amount = if (sellIn < 1) 2 else 1
      updateItem(item, sellIn - 1, increase(quality, amount))
  }

  private val updateBackstage: PartialFunction[Item, Item] = {
    case item @ Item(TYPE_BACKSTAGE, sellIn, quality) =>
      val limits = List(1 -> -quality, 6 -> 3, 11 -> 2)
      val amount = limits.find(e => sellIn < e._1) map (_._2) getOrElse 1
      updateItem(item, sellIn - 1, increase(quality, amount))
  }

  private val updateSulfuras: PartialFunction[Item, Item] = {
    case item @ Item(TYPE_SULFURAS, sellIn, quality) => item
  }

  private val updateConjured: PartialFunction[Item, Item] = {
    case item @ Item(TYPE_CONJURED, sellIn, quality) =>
      val amount = if (sellIn < 1) 4 else 2
      updateItem(item, sellIn - 1, decrease(quality, amount))
  }

  private val updateNormal: PartialFunction[Item, Item] = {
    case item @ Item(_, sellIn, quality) =>
      val amount = if (sellIn < 1) 2 else 1
      updateItem(item, sellIn - 1, decrease(quality, amount))
  }

  private def updateItem(item: Item, sellIn: Int, quality: Int) = {
    item.copy(sellIn = sellIn, quality = quality)
  }

  private def decrease(value: Int, amount: Int) = {
    if (value > amount) value - amount else 0
  }

  private def increaseWithLimit(limit: Int)(value: Int, amount: Int) = {
    val newQuality = value + amount
    if (newQuality <= limit) newQuality else limit
  }

  private val increase = increaseWithLimit(50) _
}

object GildedRose {
  val TYPE_AGED = "Aged Brie"
  val TYPE_BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert"
  val TYPE_SULFURAS = "Sulfuras, Hand of Ragnaros"
  val TYPE_CONJURED = "Conjured"
}

