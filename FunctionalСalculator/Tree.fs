module Tree

type Tree =
  | BinBranch of (double->double->double) * Tree * Tree
  | UnoBranch of (double->double) * Tree
  | Leaf of double