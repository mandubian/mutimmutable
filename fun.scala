import com.quantifind.charts.Highcharts._

def ptime[A](f: => A) = {
  val t0 = System.nanoTime
  val ans = f
  printf("Elapsed: %.9f sec\n",(System.nanoTime-t0)*1e-9)
  ans
}

def doit(l: List[Int]) = l.foldLeft((List[Int](), List[Int](), List[Int]())){ case ((a, b, c), x) =>
  (   a :+ 2*x
    , b :+ 3*x
    , c :+ 4*x
  )
}

def doitFast(l: List[Int]) = {
  val a, b, c = List.newBuilder[Int]

  l foreach { e =>
    a += e
    b += e
    b += e
  }

  (a.result(), b.result(), c.result())
}


val l1000    = (1 to 1000).toList
val l10000   = (1 to 10000).toList
val l100000  = (1 to 100000).toList
val l1000000 = (1 to 1000000).toList

ptime(append(l1000, List(1)))
ptime(append(l10000, List(1)))
ptime(append(l100000, List(1)))
ptime(append(l1000000, List(1)))

ptime(doit(l1000))

val l = List(1000, 10000, 20000, 30000, 40000, 50000, 100000, 500000, 1000000)

line(l, l map (ptime(doit(_))))