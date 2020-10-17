


// 1
def listIlo(list: List[Double]):Double = list match {
		case Nil => 0
		case head::tail => head * listIlo(tail)
}

// 1 test
listIlo(List(1,2,3))
listIlo(List())
listIlo(List(-3,2,1))
listIlo(List(1,2,3,0))

// 2
def list2sentence (list:List[String],eol:String,delimiter:String):String = list match {
	case Nil => eol
	case head::Nil => head + eol
	case head::tail => head + delimiter + list2sentence(tail,eol,delimiter)
}

// 2 test
println(list2sentence(List("Another","pointless","test","case"),".",", "))
println(list2sentence(List("Pointless"),".",", "))
println(list2sentence(List(),".",", "))

// 3
def checkInter (list:List[Int],X:Int,Y:Int):Boolean = list match {
	case Nil => true
	case head::Nil => if(head>=X && head<=Y) true else false
	case head::tail => checkInter (tail,X,Y)
}

// 3 test
checkInter(List(1,2),3,4)
checkInter(List(1,2),0,4)
checkInter(List(),0,4)
checkInter(List(5),0,4)

// 4
def potegaI(X: Int, Y: Int): Int = {
	def pom(wyn: Int, Y: Int): Int = Y match {
		case x if x<0 => 0
		case 0 => 1
		case 1 => wyn
		case _ => pom(wyn*X, Y-1)
	}
	pom(X,Y)
}	
	
def powerD(X:Double, Y: Double): Double = {
	def help(res: Double, Y: Double): Double = Y match {
		case x if x<0 => 0
		case 0 => 1
		case 1 => res
		case _ => help(res*X, Y-1)
	}
	help(X,Y)
}

// 4 test
powerD(2,3)
powerD(-2,3)
powerD(0,3)
powerD(3,0)
powerD(3,-3)
