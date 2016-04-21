"abcd".groupBy(_.toLower).mapValues(_.size).toList.sortBy(_._1)

for {
  i <- 0 to 2
} yield {
  if (i > 0) ('a', i)
  else Nil
}

