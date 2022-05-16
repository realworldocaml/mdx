
# 2.0.0 (October 2, 2020)

  - port to dune and opam 2.0
  - :exclamation: opam package now split into two packages: ocamlgraph and ocamlgraph_gtk
  - [WeakTopological] fixed incorrect use of generic hash tables
    (#99, Tomáš Dacík)
  - [Oper] fixed transitive_reduction (#91)
  - fix incorrect uses of polymorphic equality (Steffen Smolka, Boris Yakobowski)
  - [Coloring] fixed generation of OCamlDoc documentation
    (contributed by Earnestly)
  - :exclamation: [Coloring] functions now fail if the graph is directed
  - :exclamation: [Coloring] now uses a single, global exception [NoColoring]
  - [Coloring] new function two_color to 2-color a graph (or fail)
  - :exclamation: [Fixpoint] Take initial labeling of nodes into account (Johannes Kloos)

# 1.8.8, October 17, 2017

  - fixed installation (Virgile Prevosto, Jacques-Pascal Deplaix)
  - safe-string compatible (Jacques-Pascal Deplaix)
  - :exclamation: fixed method get_edge_layout of class abstract_model of DGraphModel.Make. The
    bug could have occured when there are several edges between two vertices.
  - [Traverse/Pack] added Dfs.fold and Dfs.fold_component (tail-recursive)
   (contributed by Guillaume Chelfi)
  - :exclamation: fixed implementation of Golberg-Tarjan maximal flow algorithm
    (contributed by Guyslain Naves) No more function min_capacity in
    the input interface.  Renaming as follows: Flow.Goldberg ->
    Flow.Goldberg_Tarjan and Pack.goldberg -> Pack.goldberg_tarjan
  - new functors [WeakTopological] and [ChaoticIteration] to compute
   fixpoints with widening, following Bourdoncle's algorithms (contributed
   by Thibault Suzanne)

# 1.8.7, April 12, 2016

  - fixed examples/demo.ml so that it also compiles with an installed OCamlGraph
  - [Components] fixed stack overflow with [scc] (patch by Albin Coquereau)
  - [Dominator] fixed stack overflow (patch by Albin Coquereau)
  - new functor [Path.Johnson] to compute all pairs of shortest paths
   using Johnson's algorithm (contributed by Mário Pereira)
  - fixed configuration on Windows (patch by Martin R. Neuhäußer)
  - new functor [Components.Undirected] to compute connected components
  - Graphviz: fixed printing of attribute BgcolorWithTransparency
  - :exclamation: Prim, Nonnegative: function weight now has the more general type "edge -> t"
   (to be consistent with Path)
  - new module type Sig.WEIGHT (used in Path, Prim, and Nonnegative)
  - Fixpoint: do not catch Not_found raised by a user-provided function.
  - Adding folds to BFS.

# 1.8.6, January 23, 2015

  - :exclamation: Dominator: new functor [Make_graph] with may use graph building operations,
    while the old functor [Make] now only requires a read-only graph.
    Function [compute_all] and [compute_dom_graph] are now only defined in the
    new [Make_graph] functor.
  - Graphviz: support for additional polygonal-shapes
  - New module Clique (contributed by Giselle Reis)
  - Avoid ocamldoc error with OCaml 4.02
  - :exclamation: Path: function weight now has the more general type "edge -> t"
    (contributed by Steffen Smolka)
    update your code by turning "weight l" into "weight (G.E.label e)"
  - installation: "make install-findlib" now uses DESTDIR when defined
  - Traverse: documentation is clarified: providing iterators over the roots of
    the graph is enough.
  - Imperative concreate (di)graph: fix bug of functions add_edge* of
    imperative concrete (di)graphs which appears when the added edge
    [e] was already in the graph [g] and one of the vertices of [e] is
    equal to another vertex of [g] (when using the user-defined
    equality [G.V.equal]), but not physically equal to it. This bug
    only occurs with OCaml version >= 4.0.
  - functions in modules Components, Dominator, Flow, Topological and
    Traverse now create smaller auxiliary hash tables
  - Graphviz: add the attribute `HtmlLabel to specify html strings.

# 1.8.5, March 4, 2014

  - Graphviz: reverted to the old API where edges and vertices are
    given a single style attribute; but such attributes are collected
    and output in the DOT file into a list (thus allowing multiple
    style attributes)
  - fixed issue in ./configure with ocamlfind on Win32.
  - fixed compilation when laglgnomecanvas is missing (bug introduced in 1.8.4).
  - fixed more issues with 'make -j'.

# version 1.8.4, February 4, 2014

  - fixed [Graphml] printer (contributed by Johannes Schauer)
  - Components: the components of [scc_list] are provided in the same order than
    the ones of [scc_array].
  - fix compilation with 'make -j'
  - Graphviz: handle attribute penwidth on edges and vertices
  - also install graph.o and graph.cmxs
  - fixed installation with ocamlfind (thanks to Virgile Prevosto)
  - new functions [gnp] and [gnp_labeled] in module [Rand] to generate random
    graphs using the G(n,p) model (contributed by Thomas Aubry)
  - Prim's algorithm (in module [Prim]; not exported in [Pack])
    (contributed by Thomas Aubry)
  - Graphviz: support for nested subgraphs. (Backward-incompatible change:
    add 'sg_parent = None' to obtain subgraphs whose parents are the main graph)
  - Graphviz: edges and vertices can now receive multiple styles.
    (Backward-incompatible change: constructors `Style now require a
    list as argument)
  - Graphviz: new vertex style 'rounded'
  - Merge: fixed bug with vertices with no incoming nor outcoming edge.
  - fixed compilation issue with OCaml 3.12.1

# 1.8.3, April 17, 2013

  - new module Merge implementing several different merges of vertices and
    edges into a graph (contributed by Emmanuel Haucourt)
  - fixed DOT parser (contributed by Alex Reece)
  - Topological: fixed bug in presence of disjoint cycles; new implementation
  - new module [Graphml] to export graphs into the graphml format
    (contributed by Pietro Abate)
  - Builder.S now contains functions remove_*.
  - modified Fixpoint module so that it also works with unlabeled
    graphs, break backward compatibility yet (contributed by Markus W. Weissmann)
  - support of lablgtk2 installed with findlib (contributed by B. Monate)
  - new module [Dominator] to compute dominators
   (contributed by David Brumley and Ivan Jager)

# 1.8.2, May 12, 2012

  - new module [Path.BellmanFord] implementing Bellman-Ford algorithm
    (contributed by Yuto Takei)
  - new module Contraction implementing edge contraction
    (contributed by Markus W. Weissmann)
  - Gmap: new function [filter_map] (contributed by Markus W. Weissmann)
  - Topological: fix bug when a cycle depends on another cycle. That breaks
    compatibility: the input graph must implement Sig.COMPARABLE instead of
    Sig.HASHABLE
  - new module Topological.Make_stable to iterate over a graph in a **stable**
    topological order. Stable means that the provided ordering only depends on
    the graph topology and on the user's vertices ordering.
  - new module Leaderlist to compute the leader list algorithm (see the Dragon)
    (contributed by Markus W. Weissmann <markus.weissmann@in.tum.de>)

# 1.8.1, October 17, 2011

  - module Gmap now has a signature for edges (E_SRC) compatible with
    Sig, so that it is easier to apply functor Gmap.Edge
    (contributed by Markus W. Weissmann <markus.weissmann@in.tum.de>)
  - new module Fixpoint to compute fixpoints using the work-list
    algorithm, e.g. for data flow analysis
    (contributed by Markus W. Weissmann <markus.weissmann@in.tum.de>)

# 1.8, October 4, 2011

  - removed ocamlyacc shift/reduce conflict in dot_parser.mly
    (patch contributed by Till Varoquaux)
  - Dgraph: Correct height and width-related problems with text
  - DGraph: many bug fixes. Patch by Benjamin Monate
  - Sig.G: new function [find_all_edges] for each graph implementation
  - Oper: fixed bug with function [intersect]: now use G.E.compare instead of (=)
  - fixed "make install-findlib"

# 1.7, February 23, 2011

  - Makefile: fixed bug when installing ocamlgraph with ocamlfind
  - DGraph: fixed bug with colors on some windows manager
  - Configure: fixed issue with automatic detection of extension under
    Windows/Mingw

# 1.6, December 13, 2010

  - DGraph: new viewer mode (called `tree') which focus on a subset part of the
    graph centered on a given node
  - Graphviz: fixed bug with attribute `Constraint (was `Constraints) (patch by
    Boris Yakobowski)
  - DGraph: fixed font size issue when zooming
  - DGraph: now interpret text anchors and espaced sequences correctly
  - DGraph: offer possibility to disable the default callbacks on nodes
  - 'make -j' works again
  - new implementation Persistent.Digraph.ConcreteBidirectionalLabeled
  - new implementation Persistent.Digraph.ConcreteBidirectional
  - Makefile: bug fixed when ocamlc (resp. ocamlopt) and ocamlc.opt
    (resp. ocamlopt.opt) are unconsistent

# 1.5, April 29, 2010

  - Makefile: bug fixed when installing ocamlgraph with ocamlfind
   (patch by Virgile Prevosto)
  - DGraph: new method set_zoom_padding in DgraphView.view
  - Traverse.Dfs.has_cycle: can now be used on undirected graphs as well,
   and is now tail recursive
  - DGraph: handle dotted ellipse

# 1.4, March 24, 2010

  - new function [clear] for imperative graphs
  - new implementation Imperative.Digraph.ConcreteBidirectionalLabeled
    (contribution of Jaap Boender)
  - Dgraph displays graphs faster
  - DGraph: several bugs fixed
  - DGraph: several API changes
    (may break compatibility with existing development)

# 1.3, October 2, 2009

  - Oper.mirror: undirected graphs are not copied anymore
  - Oper.mirror: fixed bug (isolated vertices were lost)
  - Graphviz: new signature GraphWithDotAttrs
  - Improvements of Dgraph
  - Configure: better test for detecting lablgtk
  - OcamlGraph is now installed by default in `ocamlc -where`/ocamlgraph
  - Viewgraph is now packed in a single module Viewgraph (break compatibility
    with previous version)
  - Dgraph is now packed in a single module Dgraph (break compatibility with
    previous version)
  - Makefile: fixed bug when the installation directory of binaries does not
             exist
  - Makefile: fixed bug of ocamldep under Windows

# 1.2, August 31, 2009

  - experimental: new GTK-based graph viewer called Dgraph
    (viewGraph is now deprecated)
  - added Delaunay.iter_triangles (not of use in Ocamlgraph, actually)

# 1.1, June 23, 2009

  - added constraint "type E.label = unit" to unlabeled graph data structure
  - viewGraph: new module viewGraph_utils; fixed compilation (gtkInit
    was missing; patch by Mehdi Dogguy)
  - configure: fixed bug under Cygwin (patch by Julien Bernet)
  - configure: look for lablgnomecanvas.cmxa when compiling in native code
  - Makefile: fixed make install-findlib when lablgtk and/or
    lablgnomecanvas not installed (patch by Peter Hawkins)

# 1.0, October 14, 2008

  - license: LGPL updated to version 2.1
  - ocamlgraph now requires ocaml 3.10 or higher
  - experimental: GTK-based graph viewer based on dot
    (contribution of Anne Pacalet)
  - Makefile:
     - fixed bug when lablgnomecanvas is not installed
     - fixed bug when ocamlfind is installed
     - patch to the build system for a DESTDIR (patch by Igor Galic)
     - "make -j" compliant
  - new function Blocks.after_unserialization in order to be able to safely
    serialize abstract vertices (see the FAQ)
  - Dot:
     - fixed bug in the parsing of attributes
     - fixed bug in the parsing of subgraphs (patch by Anne Pacalet)
  - Oper: fixed bug in intersect
  - Path: improved efficiency of Dijkstra
  - Topological:
     - fixed bug in Make.{iter,fold}
     - folding is now tail-recursive (patch by Michael Furr)

# 0.99c, April 3rd, 2008

  - replicated a bug fix of Bitv (could not affect Matrix, though)
  - fixed DFS traversal in Traverse.Dfs.{prefix,prefix_component,iterator}

# 0.99b, December 18th, 2007

  - fixed link bug with ocaml 3.09
    (see http://caml.inria.fr/mantis/view.php?id=4124)

# 0.99a, November 21st, 2007

  - fixed bug in Makefile when lablgtk2 is not installed
  - Sig.I.create and Sig_pack.create are now of type ?size:int -> unit -> t
    (instead of unit -> t) to indicate an initial guess on the graph size

# 0.99, June 27th, 2007

  - experimental: GTK-based graph editor based on hyperbolic geometry;
    to build it and test it, cd to editor/ and type make
  - [Components.Make] functor: function [scc] as a new profile and a
    more precise specification. New function [scc_array].
  - fixed configure to set ocaml's standard library using "ocamlc -where"
  - new module Dot to parse files in Graphviz's DOT format
  - slight change in the license (no more clause 6 of the LGPL; see LICENSE)
  - new module Strat implementing simple game strategies
  - Graphviz: little refactoring and improved documentation

# 0.98, Sep 27th, 2006

  - rename Sig.IA to Sig.IM
  - add subgraph support in Graphviz
    (breaks ascendent compatibility if you use Graphviz: by default add
      let get_subgraph _ = None
    in the argument of Graphviz.Dot and Graphviz.Neato)

# 0.97, July 20th, 2006

  - fixed compilation under Windows/Cygwin (contributed by Denis Berthod)
  - fixed bug with escaped string in Graphviz

# 0.96, May 2nd, 2006

  - new demo program: sudoku.ml (solving the Sudoku using graph coloring)
  - new module Coloring for (rather brute force) graph coloring
  - new implementation Imperative.Digraph.ConcreteBidirectional
    that maintains the set of ingoing edges for each node, thus improving the
    efficiency of functions such as iter_pred, pred, in_degree, etc.
    (contributed by Ted Kremenek)

# 0.95, November 2nd, 2005

  - compatibility with ocaml 3.09
  - new module Path.Check to check for paths

# 0.94, July 6th, 2005

  - new module Gml to parse and print graphs in GML file format
    (see http://www.infosun.fmi.uni-passau.de/Graphlet/GML)
    corresponding functions parse_gml_file and print_gml_file in Pack
  - added display_with_gv in Pack
  - improved implementation of Unionfind (patch by Marc Lasson)

# 0.93, March 31st, 2005

  - fixed bug in Rand (integer overflow causing Invalid_argument "random");
    improved code in Rand
  - bug fixed in the META file
  - add find_edge in Sig.G (and so in all graph implementations)

# 0.92, January 18th, 2005

  - fixed escaped labels in Graphviz output (patch by Boris Yakobowski)
  - new Graphviz option OrderingOut (patch by Boris Yakobowski)
  - fixed sharing bugs in Oper (patch by Boris Yakobowski)
  - fixed bug in nb_edges for undirected graphs
  - improvement of iterators of undirected concrete graphs

# 0.91, December 16th, 2004

  - more precise specifications of remove_edge and shortest_path.
  - bug fixed in mem_edge_e of labelled graphs
  - generation of random graphs improved
  - add Rand.Make and Rand.Planar.Make (generic functors)
  - add signatures Persistent.S and Imperative.S

# 0.90, November 30th, 2004

  - graph.cma graph.cmxa
  - version.ml and META files are now writable
  - add interfaces Sig.VERTEX and Sig.EDGE
  - "sig.ml" and "sig_pack.ml" removed; ocamlgraph now requires ocaml 3.08.0
  - improvement of Minsep
  - add Components.scc_list
  - Oper.Neighbourhood replaces Neighborhood
  - Gmap replaces Copy
  - add types Sig_pack.vertex and Sig_pack.edge
  - fixed bug in Ford-Fulkerson: G.V.equal instead of = in two asserts

# 0.81, July 13th, 2004

  - compatibility with ocaml 3.08
  - Oper.Choose.choose_edge: choose an edge in a graph
  - add types Sig.G.edge and Sig.G.vertex resp. equal to Sig.G.V.t and Sig.G.E.t
  - fixed typos in invalid_arg arguments (in Bitv)

# 0.80, June 28th, 2004

  - major contribution by Matthieu Sozeau and Pierre-Loïc Garoche.
    New modules are:
    - Md: Minimum Degree algorithm
    - Cliquetree: the clique tree of a graph
    - Mcs_m: Maximal Cardinality Search (MCS-M) algorithm
    - Minsep: Minimal separators of a graph
    - Neighborhood: compute the neighborhood of a vertex/some vertices
    - Oper.Difference: subgraphs induced by the elimination of some vertices
    - Oper.Choose: choose a vertex in a graph
    - Copy: graphs copying
    - Util.DataV: create a vertex type with data attached to it
  - out_degree: raises Invalid_argument if v not in g (instead of Not_found)
  - Pack.Graph: golberg/ford_fulkerson fail ("not a directed graph")

# 0.70, Feb 27th, 2004

  - Makefile.in: dependences ("make -j" works)
  - union and intersection (see Oper.S.union and Oper.S.intersection)
  - Golberg/Ford_fulkerson algorithms in a single module Flow
  - step-by-step iterators in Traverse.{Dfs,Bfs}
  - Ford_fulkerson: maxflow now returns a flow function over edges

# 0.60, Feb 18th, 2004

  - fixed bug in Ford-Fulkerson
  - random planar graphs (see Rand.Planar)
  - Delaunay triangulation (see Delaunay)
  - implementations with adjacency matrices (see Imperative.Matrix)
  - operations on predecessors for undirected graphs optimized
  - Traverse.Dfs.{prefix,prefix_component} optimized (now tail recursive)

# 0.50, Feb 4th, 2004

  - first release


