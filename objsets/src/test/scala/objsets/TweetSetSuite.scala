package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val e = new Tweet("e", "e body", 12)
    val f = new Tweet("f", "f body", 18)
    val g = new Tweet("g", "g body", 21)
    val h = new Tweet("h", "h body", 1)
    val i = new Tweet("i", "i body", 30)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set5 incl e incl f incl g incl h incl i
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }
  
  test("filter: > 20 on set6") {
    new TestSets {
      assert(size(set6.filter(tw => tw.retweets > 20)) === 2)
    }
  }
  
  test("filter: there are 146 tweets < 10 TweetData") {
    new TestSets {
      assert(size(TweetReader.allTweets.filter(x => x.retweets < 10 )) === 146)
    }
  }
  
  test("filter: googleTweets") {
    new TestSets {
      assert(GoogleVsApple.googleTweets.filter( x => GoogleVsApple.google.forall { y => x.text.contains(y) }).isEmpty)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("mostTweeted") {
    new TestSets {
      assert(30 == set6.mostRetweeted.retweets)
    }
  }
  
  test("mostTweeted in TweetData") {
    new TestSets {
      assert(345 == TweetReader.allTweets.mostRetweeted.retweets)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
  
  test("descending: TweetData") {
    new TestSets {
      val trends = TweetReader.allTweets.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "mashable")
      assert(trends.head.text == "'Dexter' Season 7 Premiere Available in Full on @YouTube [VIDEO] http://t.co/Cc5HeiZZ")
      assert(trends.head.retweets == 345) 
    }
  }
  
//  test("descending: GooleVsApple") {
//    new TestSets {
//      assert(!GoogleVsApple.trending.isEmpty)
//      assert(GoogleVsApple.trending.head.user == "gizmodo")
//      assert(GoogleVsApple.trending.head.text == "iPhone 5's brain dissected. Guess what, it's made by Samsung. http://t.co/wSyjvpDc")
//      assert(GoogleVsApple.trending.head.retweets == 321) 
//    }
//  }

  }
