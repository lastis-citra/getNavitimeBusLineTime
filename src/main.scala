
import java.net.SocketTimeoutException
import java.io.IOException
import org.jsoup._
import collection.JavaConversions._
import scala.io._
//import scalax.file.Path

object main {
  def main(args: Array[String]) {
    //val uri = "https://www.navitime.co.jp/diagram/bus/00257161/00050458/0/";
    val uri = "https://www.navitime.co.jp/diagram/bus/00268556/00051670/0/";
    //val uri = https://www.navitime.co.jp/diagram/bus/00018776/00005312/0/
    //val outputPath = Path("./" + uri.split("/")(5) + ".csv")
    //outputPath.write("")

    val doc = Jsoup.connect(uri).get

    // TODO: 平日，休日や上り，下りをどうするか考える
    // 平日は0，土曜は1，日曜は2
    val date = 2
    // 順方向は0，逆方面は1
    val dir = 0
    val divEleStr = "d_" + dir.toString + "_" + date.toString
    val divEle = doc.getElementById(divEleStr)
    val dlEleStr = "dl_" + date.toString

    val dlEles = divEle.children
    // ～時台，ごとに切り出す
    val nameTimeTupleListListBuf = for (dlEle <- dlEles if dlEle.className == dlEleStr) yield {
      //println(dlEle.text)
      val liEles = dlEle.child(1).child(0).children
      // 時刻1つ，ごとに切り出す
      val nameTimeTupleListBuf = for (liEle <- liEles) yield {
        val uri = "https:" + liEle.child(0).attr("href")
        //println(uri)
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
          nameTimeSeq(point)._2
        } else {
          ""
        }
      }
      timeSeq
    }

    // 表示用
    for (name <- allNameSeq) {
      // （も）も含まない0文字以上の文字列を（）で囲んだ文字列にマッチする正規表現
      // 〔〕も同様の処理
      val rename = name.replaceFirst("（[^（）]*）$", "").replaceFirst("〔[^〔〕]*〕$", "")
      print(rename + ",")
    }
    println()
    for (timeSeq <- timeSeqSeq) {
      for (time <- timeSeq) {
        print(time + ",")
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
    maxSizeNameSeq(0)
  }

  // 全リストから，次に比較するリストを取り出す
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

  def getOnePage(uri: String, outputPath: String): Seq[(String, String)] = {
    //val urlHead = uri.split("/").init.mkString("/")

    val doc = Jsoup.connect(uri).get

    val divEle = doc.getElementById("stoplist-matrix")
    val tableEles = divEle.children

    val nameTimeTupleBuf = for (tableEle <- tableEles) yield {
      val name = tableEle.getElementsByClass("name").text
      val time = tableEle.getElementsByClass("time").text.replace("発", "").replace("着", "")
      //println(name + "," + time)
      (name, time)
    }
    nameTimeTupleBuf.toSeq
  }
}