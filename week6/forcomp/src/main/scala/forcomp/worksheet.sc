val listOfLists = List(List("sane", "qw"), List("my"), List("tt", "ii"))
//listOfLists.foldRight(List[Sentence]())((sentences: List[Sentence], options: List[Word]) => {
//    sentences ++ options.map(word => )
//})

listOfLists.zipWithIndex

def combs(listsWithIndex: List[(List[Word], Int)]): List[Sentence] = listsWithIndex match {
  case Nil => List(Nil)
  case (options, index) :: rest =>
    for {
      restCombinations <- combs(rest)
      word <- options
    } yield word :: restCombinations
}

combs(listOfLists.zipWithIndex)


"Groovy".groupBy(c => c.toLower).map((tuple: (Char, String)) => (tuple._1, tuple._2.length)).toList


List("ate", "eat", "tea")
  .foldLeft(Map[Occurrences, List[Word]]() withDefaultValue List())((occ: Map[Occurrences, List[Word]], s: String) => {
    val wocc = wordOccurrences(s)
    occ.updated(wocc, s :: occ(wocc))
  })
List("ate", "eat", "tea")
  .foldRight(Map[Occurrences, List[Word]]() withDefaultValue List())((s: String, occ: Map[Occurrences, List[Word]]) => {
    val wocc = wordOccurrences(s)
    occ.updated(wocc, s :: occ(wocc))
  })
//List("ate", "eat", "tea").map((s: String) => s.sorted).groupBy((s: String) => s)

val occurrences: Occurrences = List(('a', 2), ('b', 2), ('c', 2))
val occurrences1: Occurrences = List(('a', 2))

//val elementsFlat =
//  for ((ch1, n1) <- occurrences; (ch2, n2) <- occurrences)
//    yield List((ch1, n1), (ch2, n2))

def comb(occurrences: Occurrences): List[Occurrences] = occurrences match {
  case Nil => List(Nil)
//  case Nil => List() :: Nil
  case (char, cnt) :: rest =>
    val newCombinations = for ((char, cnt) <- occurrences; i <- 1 to cnt)
      yield occurrences.map((tuple: (Char, Int)) => tuple match {
        case (ch, n) => if (ch == char) (ch, n - i) else (ch, n)
      }).filter((tuple: (Char, Int)) => tuple match {
        case (_, n) => n > 0
      })
//    val (sameLenComb, otherComb) = newCombinations.partition((occ: Occurrences) => occ.size == occurrences.size)
//    List() :: otherComb.foldLeft(occurrences :: sameLenComb)((newAcc: List[Occurrences], occ: Occurrences) => {
//      newAcc ++ comb(occ)
//    })
    (List() :: newCombinations.filter(occ => occ != Nil).foldLeft(occurrences :: Nil)((newAcc: List[Occurrences], occ: Occurrences) => {
      newAcc ++ comb(occ)
    })).toSet.toList
//    if (cnt > 1) comb((char, cnt - 1) :: rest, occurrences :: acc)
//    else comb(rest, occurrences :: acc)
}

val res = comb(occurrences)
comb(occurrences1)

res.size
res.toSet.size

//def comb(occurrences: Occurrences, n: Int): List[Occurrences] = {
//
//}

//combinations(occurrences)
//
//def combinations(occurrences: Occurrences): List[Occurrences] =
//  for (o <- occurrences)


def wordOccurrences(w: String): List[(Char, Int)] =
  w.groupBy(char => char.toLower)
    .map((tuple: (Char, String)) => (tuple._1, tuple._2.length))
    .toList
    .sortBy((tuple: (Char, Int)) => tuple._1)


type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]
