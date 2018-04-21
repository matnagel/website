{-# LANGUAGE OverloadedStrings #-}

module TopologySeminar (
topSemPage
) where

import Data.Time.Calendar
import Utils
import Utility.Seminars

yeorgosTalk18 = Talk (day 2018 03 29)
  <| speaker "Andreas Arvanitoyeorgos" "University of Patras"
  <| title "Homogeneous geodesics and two-step homogeneous geodesics in homogeneous spaces"
  <| abstract "A homogeneous Riemannian manifold $(M = G/K, g)$  is called a space with homogeneous geodesics or a $G$-g.o. space if every geodesic of $M$ is an orbit of a one-parameter subgroup of $G.$ In the present talk I will  present some recent results on g.o. manifolds based on joint works with Yu Wang, G. Zhao.  I will also introduce the concept of a two-step homogeneous geodesic, based on joint work with N.P Souris and extension to pseudo-Riemannian case."

ritaTalk18 = Talk (dayInfo 2018 04 27 "Friday 15:30")
  <| speaker "Rita Gitik" "University of Michigan"
  <| title "A new algorithm in group theory"
  <| abstract "We describe a new algorithm which determines if the intersection of a quasiconvex subgroup of a negatively curved group with any of its conjugates is infinite. The algorithm is based on the concepts of a coset graph and a weakly Nielsen generating set of a subgroup. We also give a new proof of decidability of a membership problem for quasiconvex subgroups of negatively curved groups."

elisaTalk18 = Talk (dayInfo 2018 04 13 "Friday 11:15")
  <| speaker "Elise Villella" "University of Pittsburgh"
  <| title "Virtual Gelfand-Zetlin polytopes"
  <| abstract "Gelfand-Zetlin polytopes are an important family of convex polytopes that appear in representation theory. I will define and describe the so-called virtual Gelfand-Zetlin polytopes associated to non-dominant weights. We will discuss connection between geometry of the flag variety and a certain toric variety related to Gelfand-Zetlin polytopes."

erikTalk18 = Talk (day 2018 03 22)
    <| speaker "Erik Pedersen" "University of Copenhagen"
    <| title "Controlled algebra and topological data analysis"
    <| abstract "The talk will be an introduction to controlled algebra, giving among other an indication of how to prove topological invariance of Whitehead torsion. At the end I will discuss how these ideas may be useful in topological data analysis."

yulanTalk18 = Talk (day 2018 04 05)
    <| speaker "Yulan Qing" "University of Toronto"
    <| title "Loops with large twist get short along quasi-geodesics in $Out(F_n)$"
    <| abstract "We start with the basic of geometric group theory, that is, using geometric objects to understand word problems. The group of interest is $Out(F_n)$. Given an element $\\phi$ in $Out(F_n)$, we discuss what is the \"generalized intersection number\" associated with this element. There are several natural paths connecting the origin to $\\phi$ in $Out(F_n)$, for example, a path associate to sequence of Stalling folds and paths induced by the shadow of standard geodesics in Outer space. We use the intersection number to show that neither of these paths is, in general, a quasi-geodesic in $Out(F_n)$."

changliangTalk18 = Talk (day 2018 02 01)
    <| speaker "王常亮" "McMaster University"
    <| title "Perelman's functionals on compact manifolds with isolated conical singularities"
    <| abstract (do
          "We extend the theory of the Perelman's functionals on compact smooth manifolds to compact manifolds with isolated conical singularities.  For the $\\lambda$-functional, this is essentially an eigenvalue problem for a Schrödinger operator with singular potential. We obtain a certain asymptotic behavior of eigenfunctions near the singularities. This asymptotic behavior plays an important role for deriving the variation formulas of the $\\lambda$-functional and other applications. Moreover, we show that the infimum of the $W$-functional over a suitable weighted Sobolev space on compact manifolds with isolated conical singularities is finite, and the minimizing function exists. We also obtain a certain asymptotic behavior for the minimizing function near the singularities."
          br
          "This is a joint work with Professor Xianzhe Dai.")

tiozzoTalk18 = Talk (day 2018 03 08)
  <| speaker "Giulio Tiozzo" "University of Toronto"
  <| title "Counting loxodromic elements"
  <| abstract ( do
    "In a group of isometries of a delta-hyperbolic space, a loxodromic element is one which acts with positive translation length, i.e. with north-south dynamics. Under general combinatorial conditions, we prove that loxodromic elements are generic (i.e. they have full density with respect to counting in balls for the word metric) and translation length grows linearly. We provide applications to relatively hyperbolic groups and graph products,  including right-angled Artin groups and right-angled Coxeter groups."
    br
    "This is joint work with I. Gekhtman and S. Taylor.")

nagelTalk18 = Talk (day 2018 01 25)
  <| speaker "Matthias Nagel" "McMaster University"
  <| title "Surface systems and triple linking numbers"
  <| Nothing

naylorTalk = Talk (day 2018 03 22)
    <| speaker "Patrick Naylor" "University of Waterloo"
    <| title "Surface bundles over surfaces"
    <| abstract ( do
      "Attached to every 4-manifold is a numerical invariant called the signature. At first glance this invariant might look similar to the Euler characteristic, but actually reflects much of the smooth topology. In the 1960's, both Atiyah and Kodaira constructed examples of 4-manifolds which illustrate the interesting behavior of the signature of a fiber bundle. In this talk, I will give an overview of the subject, as well as explain its applications to the minimal base genus problem, and the geography problem for 4-manifolds."
      br
      "This is joint work with Ahmet Beyaz, Sinem Onaran, and Doug Park.")

kasiaTalk = Talk (day 2018 02 15)
    <| speaker "Kasia Jankiewicz" "McGill University"
    <| title "Cubical dimension of small cancellation groups"
    <| abstract "Groups satisfying $C'(1/6)$ small cancellation condition are known to act properly and cocompactly on CAT(0) cube complexes. The cubical dimension of a group $G$ is the infimum $n$ such that $G$ acts properly on an $n$-dimensional CAT(0) cube complex. We will discuss a construction that for each n provides an example of a $C(6)$ group with the cubical dimension bounded below by $n$."

davidTalk = Talk (day 2017 11 30)
    <| speaker "David Duncan" "McMaster University"
    <| title "Some open problems in gauge theory, symplectic geometry, and low-dimensional topology"
    <| abstract "I will discuss a variety of open problems and partial\
    \ results (due to myself and others), each relating to at least two of\
    \ the fields appearing in the title. Some of the more famous open\
    \ problems will include Witten's conjecture and the Atiyah-Floer\
    \ conjecture; however, I will spend most of the time discussing more\
    \ tractable variants of these, and how they relate."

anjaTalk = Talk (day 2017 11 02)
    <| speaker "Anja Randecker" "University of Toronto"
    <| title "On Loch Ness monsters and wild singularities - a Halloween-inspired introduction to infinite translation surfaces"
    <| abstract "Finite translation surfaces are obtained by gluing finitely many polygons along parallel edges of the same length.\
    \ I will introduce a generalization of this concept and explain some issues that make the study more difficult.\
    \ This involves more complicated kinds of singularities, infinite topological type, and flows that get trapped.\
    \ Along the talk, there will be lots of examples and pictures."

aucklyTalk = Talk (day 2018 01 04)
    <| speaker "Dave Auckly" "Kansas State University"
    <| title "Stable equivalence of smoothly knotted surfaces"
    <| abstract (do
      "It is well known that there are smoothly inequivalent, objects in 4-dimensions that are topologically equivalent. Fairly general results exist stating that such objects become smoothly equivalent after some number of stabilizations. Until this past summer the only thing known about the number of stabilizations needed was an infinite collection of examples where one stabilization was enough."
      br
      "This talk will present the proof of a theorem demonstrating that when the easiest topological invariants are trivial two smooth surfaces become smoothly isotopic after just one stabilization."
      br
      "This is joint work with Kim, Melvin, Ruberman and Schwartz."
      )

chrisTalk = Talk (day 2017 11 23)
  <| speaker "Christopher Davis" "University of Wisconsin at Eau Claire"
  <| title "Concordance of knots in homology spheres and the solvable filtration"
  <| abstract "The classical topological knot concordance group $\\mathcal{C}$ consists\
  \ knots in $S^3$ modulo those which bound flat disks in the 4-ball. There is\
  \ a natural generalization, $\\widehat{\\mathcal{C}}$, the setting of knots in \
  \ homology spheres. Recently, Adam Levine showed that in the smooth \
  \ (as opposed to topological) setting the map $\\mathcal{C} \\rightarrow \\widehat{\\mathcal{C}}$\
  \ is not a surjection, producing knots in homology spheres not smoothly\
  \ concordant to any knot in $S^3$. During this talk we will produce strong\
  \ evidence that the opposite is true topologically. Namely we show that\
  \ modulo any term of the solvable filtration of knot concordance introduced\
  \ by Cochran-Orr-Teichner, the map $\\mathcal{C} \\rightarrow \\widehat{\\mathcal{C}}$ an isomorphism.\
  \ Since every known invariant of topological concordance vanishes at some\
  \ level of this filtration, our result implies that any possible difference\
  \ between $\\mathcal{C}$ and $\\widehat{\\mathcal{C}}$ not be detected by any currently\
  \ existent technology."

hillmanTalk = Talk (day 2017 10 26)
  <| speaker "Jonathan Hillman" "University of Sydney"
  <| title "Complements of closed hypersurfaces in $S^4$"
  <| abstract "We show that if a 3-manifold $M$ has an embedding in $S^4$ such that\
  \ both complementary regions have abelian fundamental groups then the latter\
  \ groups are cyclic, $\\mathbb{Z} \\oplus{\\mathbb{Z}/n\\mathbb{Z}}$ for some $n>1$,\
  \ $\\mathbb{Z}^2$ or $\\mathbb{Z}^3$.\
  \ If an homology handle $M$ with fundamental group $\\pi$ embeds in $S^4$\
  \ then the Blanchfield pairing is neutral. We show that an homology handle has an\
  \ embedding with both complementary fundamental groups abelian if and only if\
  \ the commutator subgroup $\\pi'$ is perfect, and the embedding is then essentially\
  \ unique."

tyroneTalk = Talk (day 2017 12 7)
    <| speaker "Tyrone Ghaswala" "University of Waterloo"
    <| title "Coverings, groupoids, and embedding braids in mapping class groups"
    <| abstract (do
      "Given a finite-sheeted branched cover between surfaces with\
      \ boundary, it is natural to ask whether every mapping class of the covering\
      \ surface projects to the base surface, and whether every mapping class of\
      \ the base surface lifts to the covering surface."
      br
      "In this talk we will address these questions, with automorphisms of\
      \ fundamental groupoids playing an integral role.  As a consequence of the\
      \ main result, we construct new embeddings of the braid group in the mapping\
      \ class group of a surface."
      br
      "This is joint work with Alan McLeay.")

katTalk = Talk (day 2017 11 9)
  <| speaker "Katherine Raoux" "Michigan State University"
  <| title "$\\tau$-invariants for knots in rational homology spheres"
  <| abstract "Using the knot filtration on the Heegaard Floer chain complex,\
    \ Ozsváth and Szabó defined an invariant of knots in the three sphere called\
    \ $\\tau(K)$, which they also showed is a lower bound for the 4-ball genus.\
    \ Generalizing their construction, I will show that for a (not necessarily\
    \ null-homologous) knot, $K$, in a rational homology sphere, $Y$, we obtain\
    \ a collection of $\\tau$-invariants, one for each spin-c structure on $Y$.\
    \ In addition, these invariants can be used to obtain a lower bound on the\
    \ genus of a surface with boundary $K$ properly embedded in a negative\
    \ definite 4-manifold with boundary $Y$."

georgeTalk = Talk (day 2017 10 12)
  <| speaker "George Dragomir" "McMaster University"
  <| title "Metric transforms yielding Gromov hyperbolic spaces"
  <| abstract "A real valued function of one variable $\\varphi$ is called a\
  \ metric transform if for every metric space $(X,d)$ the composition\
  \ $d_\\varphi = \\varphi\\circ d$ is also a metric on $X$. In this talk we\
  \ present a complete characterization of the class of approximately\
  \ nondecreasing, unbounded metric transforms $\\varphi$ such that the\
  \ half line $[0,\\infty)$ with the transformed Euclidean metric\
  \ $\\varphi(|x-y|)$ is Gromov hyperbolic. As an application we obtain a\
  \ type of rigidity with respect to metric transformations of roughly\
  \ geodesic Gromov hyperbolic spaces. This is based on joint work with\
  \ Andrew Nicas."

savelievTalk = Talk (day 2017 11 17)
  <| speaker "Nikolai Saveliev" "University of Miami"
  <| title "On gauge theoretic invariants of homology $S^1 \\times S^3$"
  <| abstract "I will survey some recent joint work on invariants of smooth\
  \ 4-manifolds with homology of $S^1 \\times S^3$. In our older work with Tom Mrowka\
  \ and Daniel Ruberman we defined a Seiberg-Witten invariant of such manifolds.\
  \ I will present a new formula (from our recent work with Jianfeng Lin and\
  \ Daniel Ruberman) which expresses this invariant in terms of the monopole\
  \ Floer homology and leads to some interesting topological applications."

matthiasTalk = Talk (day 2017 10 19)
  <| speaker "Matthias Nagel" "McMaster University"
  <| title "Concordance and the Gordon-Litherland lattice"
  <| Nothing

klausTalk = Talk (day 2017 10 2)
  <| speaker "Klaus Kröncke" "University of Hamburg"
  <| title "Stability of ALE Ricci-flat manifolds under Ricci-flow"
  <| abstract "We prove that if an ALE Ricci-flat manifold $(M,g)$  is linearly stable and integrable, it is dynamically\
    \ stable under Ricci flow, i.e. any Ricci flow starting close to g exists for all time and converges modulo\
    \ diffeomorphism to an ALE Ricci-flat metric close to g. By adapting Tian's approach in the closed case,\
    \ we show that integrability holds for ALE Calabi-Yau manifolds which implies that they are dynamically stable.\
    \ This is joint work with Alix Deruelle."

kunduriTalk = Talk (day 2017 09 21)
  <| speaker "Hari Kunduri" "Memorial University of Newfoundland"
  <| title "Geometric inequalities for black hole initial data sets"
  <| abstract "The standard picture of gravitational collapse leads (using heuristic arguments) to an inequality relating the ADM mass\
    \ and angular momentum of axisymmetric spacetimes.  Such geometric inequalities have been rigorously proved for axisymmetric,\
    \ asymptotically flat maximal initial data for the vacuum Einstein equations in dimension d=4. After a broad review, I will\
    \ discuss recent work on extending this class of inequalities to d>4, where a number of qualitative differences arise\
    \ (e.g. black holes can have non-spherical horizon topology).  In particular, I will discuss how a lower bound for the mass, in terms of\
    \ a (regularized) harmonic energy functional is obtained.  The unique minimizer of this energy is then shown to correspond to extreme black\
    \ hole initial data for fixed angular momenta. This provides a variational characterization of extreme black hole."

steveTalk = Talk (day 2017 09 14)
  <| speaker "Steven Boyer" "UQAM"
  <| title "Branched Covers of Quasipositive Links and L-Spaces."
  <| abstract "We show that if $L$ is an oriented strongly quasipositive link other than the trivial knot or a\
\ link whose Alexander polynomial is a positive power of $(t-1)$, or a quasipositive link with non-zero\
\ smooth 4-ball genus, then the Alexander polynomial and signature function of $L$ determine an\
\ integer $n(L) \\geq 1$ such that $\\Sigma_n(L)$, the n-fold cyclic cover of $S^3$ branched over $L$,\
\ is not an L-space for $n \\geq n(L)$.\
\ If $K$ is a strongly quasipositive knot with monic Alexander polynomial such as an L-space knot,\
\ we show that $\\Sigma_n(K)$ is not an L-space for $n \\geq 6$, and that the Alexander polynomial of K is a non-trivial\
\ product of cyclotomic polynomials if $\\Sigma_n(K)$ is an L-space for some $n = 2,3,4,5$. Our results allow us to\
\ calculate the smooth and topological 4-ball genera of, for instance, quasi- alternating quasipositive\
\ links. They also allow us to classify strongly quasipositive alternating links and strongly quasipositive\
\ alternating 3-strand pretzel links. This is joint work with Michel Boileau and Cameron Gordon."

topSemPage :: Day -> Html
topSemPage dd = page "Geometry & Topology Seminar" $ do
        mathjax
        pageTitle "Geometry & Topology Seminar"
        topSemContent dd

talks :: [Talk]
talks = sortTalks [
          yulanTalk18,
          ritaTalk18,
          elisaTalk18,
          yeorgosTalk18,
          erikTalk18,
          naylorTalk,
          tiozzoTalk18,
          kasiaTalk,
          changliangTalk18,
          nagelTalk18,
          aucklyTalk,
          tyroneTalk,
          davidTalk,
          chrisTalk,
          savelievTalk,
          katTalk,
          anjaTalk,
          hillmanTalk,
          georgeTalk,
          matthiasTalk,
          klausTalk,
          kunduriTalk,
          steveTalk ]

pastTalks dd = reverse $ filter ( before dd ) talks

futureTalks dd = filter ( not . before dd ) talks

topSemContent :: Day -> Html
topSemContent dd = do
    image "./images/boysurface.png" "Mesh of Boy's surface" "boysurface"
    headline "Location"
    "HH 312"
    br
    "Thursdays, 15:30 - 16:30"
    headline "Upcoming talks"
    traverse renderTalk (futureTalks dd)
    headline "Past talks"
    traverse renderTalk (pastTalks dd)
    return ()
