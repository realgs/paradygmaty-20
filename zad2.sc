def sentenceFun(words:List[String],separator:String,sign:String): String = {
  if (words != Nil)
    if (words.tail != Nil) words.head + separator + sentenceFun(words.tail,separator,sign)
    else words.head + sign
  else ""
}

sentenceFun(List("Ala","ma","Kota")," ","!") == "Ala ma Kota!"
sentenceFun(List("Czarodziej","Maciej","wywrozyl","z","tarota")," ","?") == "Czarodziej Maciej wywrozyl z tarota?"
sentenceFun(List(),"","") == ""
sentenceFun(List(),"<separatorTest>","????") == ""
sentenceFun(List("bylo","fajnie")," ","?") == "bylo fajnie?"