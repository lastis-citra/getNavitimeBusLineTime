import org.jsoup._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
//import scalax.file.Path

/**
 * arg0: URL
 * arg1: 平日は0，土曜は1，日曜は2
 * arg2: 順方向は0，逆方面は1
 * sbt run https://www.navitime.co.jp/diagram/bus/00136821/00031734/0/ 1 0
 **/
object main {
  def main(args: Array[String]) {
    //val uri = "https://www.navitime.co.jp/diagram/bus/00257161/00050458/0/";
    //val uri = "https://www.navitime.co.jp/diagram/bus/00136821/00031734/0/";
    //val uri = https://www.navitime.co.jp/diagram/bus/00018776/00005312/0/
    val uri = args(0)
    //val outputPath = Path("./" + uri.split("/")(5) + ".csv")
    //outputPath.write("")

    val doc = Jsoup.connect(uri).get

    // TODO: 平日，休日や上り，下りをどうするか考える
    // 平日は0，土曜は1，日曜は2
    //val date = 1
    val date = args(1)
    // 順方向は0，逆方面は1
    //val dir = 0
    val dir = args(2)
    val divEleStr = "d_" + dir + "_" + date
    val divEle = doc.getElementById(divEleStr)
    val dlEleStr = "dl_" + date

    val dlEles = divEle.children
    // ～時台，ごとに切り出す
    val nameTimeTupleListListBuf = for (dlEle <- dlEles.asScala if dlEle.className == dlEleStr) yield {
      println(dlEle.text)
      val liEles = dlEle.child(1).child(0).children
      // 時刻1つ，ごとに切り出す
      val nameTimeTupleListBuf = for (liEle <- liEles.asScala) yield {
        val uri = "https:" + liEle.child(0).attr("href")
        println(uri)
        getOnePage(uri, "")
      }
      nameTimeTupleListBuf.toList
    }
    // このテーブルに含まれるすべてのバスの停車駅と時刻の組を取得
    val nameTimeTable = nameTimeTupleListListBuf.flatten.toSeq
    //println("All: " + nameTimeTable.size)

    val firstNameSeq = createFirstNameSeq(nameTimeTable)
    //println(firstNameSeq)

    // このテーブルに含まれるバスのすべての停車駅のリストを作成
    val allNameSeq = createNameSeq(firstNameSeq, 0, nameTimeTable)

    //println(allNameSeq)

    // このテーブルに含まれるバスのすべての停車時刻のリストを作成
    val timeSeqSeq = for (nameTimeSeq <- nameTimeTable) yield {
      val checkNameSeq = for (nameTimeTuple <- nameTimeSeq) yield {
        nameTimeTuple._1
      }
      // allNameSeqに含まれる停車駅の時刻データがあれば時刻を，なければ空白を入れたリストを作成
      val timeSeq = for (name <- allNameSeq) yield {
        if (checkNameSeq.contains(name)) {
          val point = checkNameSeq.indexOf(name)
          val time = nameTimeSeq(point)._2
          // 着発の両方が設定されているバス停があれば，そのバス停の時刻を2つにわける
          // 片方のみ設定されている場合は，すべて先頭に時刻を入れる
          if (time.contains(" ")) {
            val tmp = time.split(" ")
            (tmp(0), tmp(1))
          } else {
            (time, "")
          }
        } else {
          ("", "")
        }
      }
      timeSeq
    }

    // 着発表示を作る
    val allStrEndSeq = for (i <- timeSeqSeq.head.indices) yield {
      checkStrEnd(i, timeSeqSeq)
    }

    // 着発表示に合わせてバス停名を調整する
    val allNameTupleSeq2 = for (i <- allStrEndSeq.indices) yield {
      if (allStrEndSeq(i)._2 != "") {
        (allNameSeq(i), allNameSeq(i))
      } else {
        (allNameSeq(i), "")
      }
    }

    // 表示用
    for (nameTuple <- allNameTupleSeq2) {
      // バス停名の（福井県）や〔東福バス〕などを削除する
      // （も）も含まない0文字以上の文字列を（）で囲んだ文字列にマッチする正規表現
      // 〔〕も同様の処理
      val rename = nameTuple._1.replaceFirst("（[^（）]*）$", "").replaceFirst("〔[^〔〕]*〕$", "")
      if (nameTuple._2 != "") {
        print(rename + "," + rename + ",")
      } else {
        print(rename + ",")
      }
    }
    println()
    for (strEnd <- allStrEndSeq) {
      if (strEnd._2 != "") {
        print(strEnd._1 + "," + strEnd._2 + ",")
      } else {
        print(strEnd._1 + ",")
      }
    }
    println()
    for (timeTupleSeq <- timeSeqSeq) {
      for (timeTuple <- timeTupleSeq) {
        if (timeTuple._2 != "") {
          print(timeTuple._1 + "," + timeTuple._2 + ",")
        } else {
          print(timeTuple._1 + ",")
        }
      }
      println()
    }
  }

  // 最も停車駅が多いものを初期のリストにする
  def createFirstNameSeq(nameTimeTable: Seq[Seq[(String, String)]]): Seq[String] = {
    val sizeSeq = for (nameTimeSeq <- nameTimeTable) yield {
      nameTimeSeq.size
    }
    val maxSize = sizeSeq.max

    val maxSizeNameSeq = for (nameTimeSeq <- nameTimeTable if nameTimeSeq.size == maxSize) yield {
      for (nameTime <- nameTimeSeq) yield {
        nameTime._1
      }
    }
    maxSizeNameSeq.head
  }

  // 全リストから，次に比較するリストを取り出す
  @tailrec
  def createNameSeq(oldNameSeq: Seq[String], checkPoint: Int, nameTimeTable: Seq[Seq[(String, String)]]): Seq[String] = {
    //println(checkPoint)

    val checkNameTimeSeq = nameTimeTable(checkPoint)
    val checkNameSeq = for (checkNameTimeTuple <- checkNameTimeSeq) yield {
      checkNameTimeTuple._1
    }
    val newNameSeq = createNameSeqOne(oldNameSeq, 0, checkNameSeq)
    if (checkPoint + 1 < nameTimeTable.size) {
      createNameSeq(newNameSeq, checkPoint + 1, nameTimeTable)
    } else {
      newNameSeq
    }
  }

  // 古いリストと新しいリストを比較する
  // 古いリストにない駅があった場合は，その駅を間に挿入し，新しいリストとして返す
  @tailrec
  def createNameSeqOne(oldNameSeq: Seq[String], checkPoint: Int, checkNameSeq: Seq[String]): Seq[String] = {
    val checkName = checkNameSeq(checkPoint)
    val newNameSeq = if (!oldNameSeq.contains(checkName)) {
      //println(checkName)
      val checkPoint = checkNameSeq.indexOf(checkName)
      val newSeq = if (checkPoint > 0) {
        val preCheckName = checkNameSeq(checkPoint - 1)
        if (oldNameSeq.contains(preCheckName)) {
          //println("test")
          //println(oldNameSeq)
          val splitPoint = oldNameSeq.indexOf(preCheckName)
          //println(splitPoint)
          val oldSeqTuple = oldNameSeq.splitAt(splitPoint + 1)
          //println(oldSeqTuple._1)
          (oldSeqTuple._1 :+ checkName) ++: oldSeqTuple._2
        } else {
          checkName +: oldNameSeq
        }
      } else {
        checkName +: oldNameSeq
      }
      newSeq
    } else {
      oldNameSeq
    }

    if (checkPoint + 1 < checkNameSeq.size) {
      createNameSeqOne(newNameSeq, checkPoint + 1, checkNameSeq)
    } else {
      newNameSeq
    }
  }

  // i番目のバス停が着発なのか発だけなのか調べる
  // 着発の両方が設定されているバス停は("着", "発")を返す
  // 最後のバス停は("着", "")を返す
  // それ以外のバス停は("発", "")を返す
  def checkStrEnd(i: Int, timeSeqSeq: Seq[Seq[(String, String)]]): (String, String) = {
    // 検索用に2番目に時刻が入っていればtrue，それ以外はfalseが入ったSeqを作っておく
    val checkStrSeq = for (timeSeq <- timeSeqSeq) yield { if (timeSeq(i)._2 != "") { true } else { false } }
    if (checkStrSeq.contains(true)) { ("着", "発") } else {
      if (i == timeSeqSeq.head.size - 1) { ("着", "") } else { ("発", "") }
    }
  }

  def getOnePage(uri: String, outputPath: String): Seq[(String, String)] = {
    //val urlHead = uri.split("/").init.mkString("/")

    val doc = Jsoup.connect(uri).get

    val divEle = doc.getElementsByClass("stops-area")
    val tableEles = divEle.asScala.head.children

    val nameTimeTupleBuf = for (tableEle <- tableEles.asScala) yield {
      val name = tableEle.getElementsByClass("station-name").text
      val time = tableEle.getElementsByClass("time").text.replace("発", "").replace("着", "")
      //println(name + "," + time)
      (name, time)
    }
    nameTimeTupleBuf.toSeq
  }
}