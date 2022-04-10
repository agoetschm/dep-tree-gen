import $ivy.`io.get-coursier::coursier:2.1.0-M5`
import coursier.core.Authentication
import coursier.maven.MavenRepository
import coursier.Resolve
import coursier._

val confluentRepo = MavenRepository("https://packages.confluent.io/maven/")
// val privateRepo = MavenRepository(
//   "https://your-private-repo",
//   Some(Authentication(sys.env("USERNAME"), sys.env("PASSWORD")))
// )

val baseResolution =
  Resolve()
    .addRepositories(confluentRepo)
// .addRepositories(privateRepo)

case class GitRepo(name: String, modules: Seq[Dependency])
object GitRepo {
  def apply(name: String, module: Dependency): GitRepo =
    GitRepo(name, Seq(module))
}

val repos = Seq(
  GitRepo(
    "github.com/scala/scala-lang",
    dep"org.scala-lang:scala3-library_3:latest.release"
  ),
  GitRepo(
    "github.com/softwaremill/tapir/",
    Seq(
      dep"com.softwaremill.sttp.tapir:tapir-core_3:latest.release",
      dep"com.softwaremill.sttp.tapir:tapir-openapi-docs_3:latest.release"
    )
  )
)

// get the dependencies of each repo ------------------------------------------

val gitRepoFromModuleName: Map[ModuleName, GitRepo] =
  (for {
    repo <- repos
    module <- repo.modules
  } yield (module.module.name, repo)).toMap

def dependenciesOf(gitRepo: GitRepo): Seq[GitRepo] = {
  val resolution =
    baseResolution
      .addDependencies(gitRepo.modules: _*)
      .run()
  val minimalSet = resolution.minDependencies
  minimalSet
    // .filter(dep => dep.module.organization.value == "your.org")
    .map((dep: Dependency) =>
      gitRepoFromModuleName.getOrElse(
        dep.module.name, {
          // println(s"missing repo for internal dependency: ${dep.module}")
          gitRepo
        }
      )
    )
    .toSeq
    .filterNot(_ == gitRepo)
}

val dependencies: Map[String, Seq[String]] =
  repos.map(repo => (repo.name, dependenciesOf(repo).map(_.name))).toMap

// build graph ----------------------------------------------------------------

case class Edge(src: String, dst: String) // dst depends on src
val edges = for {
  repo <- dependencies.keySet.toSeq
  dep <- dependencies(repo)
} yield Edge(dep, repo)

case class Node(
    name: String,
    edges: Seq[Edge]
) // edges containes the edges which have this node as src
val nodes: Seq[Node] = edges
  .groupBy(_.src)
  .toSeq
  .map { case (name, edges) => Node(name, edges) }

// let's count the incoming edges per node to get the root nodes
val incomingEdgeCount: Seq[(String, Int)] =
  nodes.map { node =>
    (node.name, edges.count(_.dst == node.name))
  }
val roots: Seq[String] =
  incomingEdgeCount
    .filter(_._2 == 0)
    .map(_._1)

type Graph =
  Map[String, Seq[String]] // this is a map from each repo to its dependents

val virtualRoot =
  "root" // this virtual root is connected to the roots and helps us to run BFS
val graph: Graph =
  (
    (virtualRoot, roots)
      +: nodes.map(n => (n.name, n.edges.map(_.dst)))
  ).toMap

// get depth of each repo in tree (color of node) -----------------------------

// get the max distance to the root to determine the depth of each repo in the dependency tree
@scala.annotation.tailrec
def maxDists(dists: Map[String, Int], toVisit: List[String]): Map[String, Int] =
  if (toVisit.isEmpty)
    dists
  else
    graph.get(toVisit.head) match {
      case None => maxDists(dists, toVisit.tail)
      case Some(neighbors) =>
        val dist = dists(toVisit.head)
        val distUpdates = neighbors.map(u => (u, dist + 1))
        maxDists(
          dists = dists ++ distUpdates,
          toVisit = toVisit.tail ++: neighbors.toList
        )
    }

val depths: Map[String, Int] = maxDists(
  dists = Map(virtualRoot -> 0),
  toVisit = List(virtualRoot)
)

// write graph to file --------------------------------------------------------

val colors = Seq(
  "dodgerblue3",
  "aquamarine4",
  "chartreuse4",
  "darkolivegreen3",
  "yellow",
  "darkorange",
  "firebrick3"
)

val leaves: Set[String] = graph.values.flatten.toSet -- graph.keySet
val dotNodes = (graph.keySet.toSeq ++ leaves.toSeq).map { repo =>
  s""""$repo" [label="$repo" color=${colors(
    depths(repo)
  )} shape=rectangle style=rounded];"""
}

val dotRanks = depths
  .groupBy(_._2)
  .toSeq
  .sortBy(_._1)
  .map { case (_: Int, nodes: Map[String, Int]) =>
    s"""|  {
      |    rank=same;
      |    ${nodes.keySet.toSeq.map(s => s"\"$s\"").mkString(";")};
      |  }""".stripMargin
  }
  .mkString("\n")

val dotEdges = graph
  .map { case (src, dsts) =>
    val formated = dsts.map(dst => s""""$dst"""")
    s"""|  "$src" -> {
        |    ${formated.mkString("\n  ")}
        |  };""".stripMargin
  }
  .mkString("\n")

val dot = s"""|digraph "generated-graph" {
              | graph [ranksep=1, rankdir=LR];
              |  ${dotNodes.mkString("\n  ")}
              |
              |  ${dotRanks}
              |
              |  ${dotEdges}
              |}""".stripMargin

println(dot)
