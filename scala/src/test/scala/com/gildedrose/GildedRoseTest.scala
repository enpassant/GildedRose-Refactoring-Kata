package com.gildedrose

import org.scalatest._

class GildedRoseTest  extends FlatSpec with Matchers {
  "Normal items" should "decrease quality and sellIn" in {
    val items = Array[Item](new Item("Normal", 5, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Normal")
    app.items(0).sellIn should equal (4)
    app.items(0).quality should equal (9)
  }

  it should "the Quality of an item is never negative" in {
    val items = Array[Item](new Item("Normal", 5, 0))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Normal")
    app.items(0).sellIn should equal (4)
    app.items(0).quality should equal (0)
  }

  it should "once the sell by date has passed, Quality degrades twice as fast" in {
    val items = Array[Item](new Item("Normal", 0, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Normal")
    app.items(0).sellIn should equal (-1)
    app.items(0).quality should equal (8)
  }

  it should "once the sell by date has passed, Quality degrades twice as fast, never negative" in {
    val items = Array[Item](new Item("Normal", 0, 1))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Normal")
    app.items(0).sellIn should equal (-1)
    app.items(0).quality should equal (0)
  }

  "Aged Brie items" should "increase quality and decrease sellIn" in {
    val items = Array[Item](new Item("Aged Brie", 5, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Aged Brie")
    app.items(0).sellIn should equal (4)
    app.items(0).quality should equal (11)
  }

  it should "the Quality of an item is never more than 50" in {
    val items = Array[Item](new Item("Aged Brie", 5, 50))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Aged Brie")
    app.items(0).sellIn should equal (4)
    app.items(0).quality should equal (50)
  }

  "Sulfuras, Hand of Ragnaros items" should "never has to be sold or decreases in Quality" in {
    val items = Array[Item](new Item("Sulfuras, Hand of Ragnaros", 5, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Sulfuras, Hand of Ragnaros")
    app.items(0).sellIn should equal (5)
    app.items(0).quality should equal (10)
  }

  "Backstage passes to a TAFKAL80ETC concert items" should "increase quality and decrease sellIn" in {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 15, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Backstage passes to a TAFKAL80ETC concert")
    app.items(0).sellIn should equal (14)
    app.items(0).quality should equal (11)
  }

  it should "the Quality of an item is never more than 50" in {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 5, 50))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Backstage passes to a TAFKAL80ETC concert")
    app.items(0).sellIn should equal (4)
    app.items(0).quality should equal (50)
  }

  it should "Quality increases by 2 when there are 10 days or less" in {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 8, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Backstage passes to a TAFKAL80ETC concert")
    app.items(0).sellIn should equal (7)
    app.items(0).quality should equal (42)
  }

  it should "Quality increases by 2 when there are 10 days or less, never more than 50" in {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 8, 49))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Backstage passes to a TAFKAL80ETC concert")
    app.items(0).sellIn should equal (7)
    app.items(0).quality should equal (50)
  }

  it should "Quality increases by 3 when there are 5 days or less" in {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 5, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Backstage passes to a TAFKAL80ETC concert")
    app.items(0).sellIn should equal (4)
    app.items(0).quality should equal (43)
  }

  it should "Quality increases by 3 when there are 10 days or less, never more than 50" in {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 5, 48))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Backstage passes to a TAFKAL80ETC concert")
    app.items(0).sellIn should equal (4)
    app.items(0).quality should equal (50)
  }

  it should "Quality drops to 0 after the concert" in {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 0, 48))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("Backstage passes to a TAFKAL80ETC concert")
    app.items(0).sellIn should equal (-1)
    app.items(0).quality should equal (0)
  }

  //"Conjured items" should "decrease quality and sellIn" in {
    //val items = Array[Item](new Item("Conjured", 5, 10))
    //val app = new GildedRose(items)
    //app.updateQuality()
    //app.items(0).name should equal ("Conjured")
    //app.items(0).sellIn should equal (4)
    //app.items(0).quality should equal (8)
  //}

  //it should "the Quality of an item is never negative" in {
    //val items = Array[Item](new Item("Conjured", 5, 0))
    //val app = new GildedRose(items)
    //app.updateQuality()
    //app.items(0).name should equal ("Conjured")
    //app.items(0).sellIn should equal (4)
    //app.items(0).quality should equal (0)
  //}

  //it should "once the sell by date has passed, Quality degrades twice as fast" in {
    //val items = Array[Item](new Item("Conjured", 0, 10))
    //val app = new GildedRose(items)
    //app.updateQuality()
    //app.items(0).name should equal ("Conjured")
    //app.items(0).sellIn should equal (-1)
    //app.items(0).quality should equal (6)
  //}

  //it should "once the sell by date has passed, Quality degrades twice as fast, never negative" in {
    //val items = Array[Item](new Item("Conjured", 0, 3))
    //val app = new GildedRose(items)
    //app.updateQuality()
    //app.items(0).name should equal ("Conjured")
    //app.items(0).sellIn should equal (-1)
    //app.items(0).quality should equal (0)
  //}
}
