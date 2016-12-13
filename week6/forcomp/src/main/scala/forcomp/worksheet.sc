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

val occurrences: Occurrences = List(('a', 2), ('b', 2))

for ((char, cnt) <- occurrences; i <- 1 to cnt) yield (char, i)

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
