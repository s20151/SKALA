// skrable  
    val slovo = "Ala ma kota"

    def countPoints(z: Char): Int = {
      z match {
        case 'a' | 'e' | 'i' | 'n' | 'o' | 'r' | 's' | 'w' | 'z' => 1
        case 'c' | 'd' | 'k' | 'l' | 'm' | 'p' | 't' | 'y' => 2
        case 'b' | 'g' | 'h' | 'j' | 'ł' | 'u' => 3
        case 'ą' | 'ę' | 'f' | 'ó' | 'ś' | 'ż' => 5
        case 'ć' => 6
        case 'ń' => 7
        case 'ź' => 9
        case _ => 0

      }
    }

    println(slovo.map(z => countPoints(z)).sum)

// odwrotny numer
    def opposite(number: Double): Double = -number

// karta kredytowa - cztery ostatnie cyfry odkryte
    def maskify(strs: String): String = 
    val lastFour = strs.toList.takeRight(4).mkString
    val rest = strs.toList.dropRight(4).map(_ => "#").mkString

    rest + lastFour
    
// wyciąganie INT z listy
    def filterList(list: List[Any]): List[Int] =
        {
          return list.collect{case x: Int => x}
        }
     
// sortowanie numerkow
    def sol(nums: List[Int]): List[Int] = {
          return nums.sorted
        }
        
// max i min z listy
    def minimum(lst: Seq[Int]): Int = lst.min
    def maximum(lst: Seq[Int]): Int = lst.max
    
    
// odwracanie wartosci liczb z listy 5 -> -5
    def invert(lst: List[Int]): List[Int] = {
      lst.map(_ * -1)
    }
    
    
// jaki znak się najczęściej powtarza
    def Repetition(word: String): (String, Int) = {
        if(word.isEmpty) ("",0) else word.span(_ == word.head) match {
          case (a, b) => Seq((a.head.toString, a.size), Repetition(b)).maxBy(_._2)
        }
      }
      
// zwroc srodkowy znak ze slowa, jak nie ma srodkowego to dwa srodkowe
    def middle(string: String): String =
      if (string.length <= 2) string
      else middle(string drop 1 dropRight 1)

// funkcja powtarzająca stringa N razy
    def repeatStr(times: Int, str: String): String = {
      val result = str*times
      return result
    }
    
// nieparzyste liczby poniżej N
    def oddCount(n: Long): Long = n / 2

// odwroc slowa w zdaniu 
    def reverseWords(str: String): String =
      str.split(' ').reverse.mkString(" ")
      
// pierwszy numer z poza sekwencji np. 1,2,[4]
    def firstNonConsecutive(xs: Seq[Int]): Option[Int] =
      xs.sliding(2).collectFirst { case Seq(x, y) if y != x+1 => y }
      
// zamiana booleana na "yes" lub "no"
    def boolToWord(boolean: Boolean): String =
      if (boolean) "Yes" else "No"
      
// zwróć wiek z roku 1999 -> 20
    def centuryFromYear(year: Int): Int = (year + 99) / 100

// prosty kalkulator np. [5, 2, "add" --> 7]
    def arithmetic(a: Int, b: Int, operator: String): Double = {
        operator match
          case "add" => a + b
          case "subtract" => a - b
          case "multiply" => a * b
          case "divide" => a.toDouble / b
    }

// zlicz wystąpienia liter w stringu
    def orderedCount(chars: String): List[(Char, Int)] = {
      chars.distinct.map(c=>(c,chars.count(_ == c))).toList
    }

// zamiana cyfty na slowo 
    val nums = Map((0 -> "Zero"), (1 -> "One"), (2 -> "Two"), (3 -> "Three"), (4 -> "Four"),
                              (5 -> "Five"), (6 -> "Six"), (7 -> "Seven"), (8 -> "Eight"), (9 -> "Nine"))
    def switchItUp(number: Int): String = nums(number)
    
// suma liczb dodatnich
    def positiveSum(arr: Array[Int]): Int = arr.filter(_ > 0).sum

// usuń co drugi element z listy
    def removeEveryOther[T](list: List[T]): List[T] =
    list.zipWithIndex.collect { case (x, i) if i%2==0 => x }
    
// każde słowo z wielkiej litery 
    extension (s: String) def toJadenCase = s.toLowerCase.split(' ').map(_.capitalize).mkString(" ")

// uporządkuj liczby malejąco 42145 -> 54421
    def descendingOrder(n: Int): Int =
        n.toString.sorted.reverse.toInt

// odliczanie liczby; input: 3 -> out: [1,2,3]
    def monkeyCount(n: Int): Array[Int] = Array.range(1, n + 1)

// dodawanie binarne
    def addBinary(a: Int, b: Int): String = {
      (a + b).toBinaryString
    }

// liczenie samoglosek
    def getCount(inputStr: String): Int = {
        val vowels = Set('a', 'e', 'i', 'o', 'u')
        inputStr.filter(vowels.contains).length
   }

// odwroc slowo (tylko litery)
      def reverseLetter(str: String): String = {
        str.replaceAll("[^A-Za-z]|_", "").reverse
      }

// liczenie owiec 
     def countingSheep(num: Int): String = {
          (1 to num).map(x => s"$x sheep...").mkString
        }

// sprawdzanie zawartosci tabeli
    def check(seq:List[Any], elem: Any) = seq.contains(elem)

// liczba do kwadratu
    def square(n: Int): Int = n * n

// silnia 
      def !(n: Int): BigInt = {
        def factHelper(num: Int, acc: BigInt = 1): BigInt = {
          if (num <= 1) return acc
          else factHelper(num - 1, acc * num)
        }
        factHelper(n)
      }

// usuń pierwszy i ostatni znak
      def removeChars(s: String): String = {
        s.tail.init
      }

// convert string to int
    def stringToNumber(s: String): Int =
     s.toIntOption.getOrElse(0)

// palindrom np. kajak 

   def isPalindrome(s: String): Boolean =
     s.toLowerCase == s.toLowerCase.reverse

// inicjaly Adam Kupa -> A.K
    def abbrevName(name: String): String =
      val s"$x $y" = name.toUpperCase
      s"${x.head}.${y.head}"

// rok przestępny
    def isLeap(year: Int): Boolean = year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)

// suma kwadratów liczb z listy
    def squareSum(xs: List[Int]): Int = xs.fold(0)((a,b) => a+b*b)

// srednia
    def getAverage(marks: List[Int]): Int = marks.sum / marks.length

// dzielniki liczby 
    def divisors(n: Int): Int = (1 to n).count(n % _ == 0)

// czy string jest z tylko wielkich liter
    def isUpperCase(s: String): Boolean = s.forall(!_.isLower)

// taka sama liczba X i O w liscie -> true
    def xo(str: String): Boolean =
        str.count(_.toLower == 'x') == str.count(_.toLower == 'o')


   
