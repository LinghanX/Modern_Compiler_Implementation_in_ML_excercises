structure Tree =
struct
  type key = string 
  datatype 'a tree = Leaf | Tree of 'a tree * 'a * 'a tree 
  val empty = Leaf

  fun insert (key, Leaf) = Tree(Leaf, key, Leaf)
    | insert (key, Tree(left, k, right)) = 
        if key < k then Tree (insert (key, left), k, right)
        else if key > k then Tree (left, k, insert (key, right))
        else Tree (left, key, right)

  fun member (key, Leaf) = false 
    | member (key, Tree(left, k, right)) = 
        if key < k then member (key, left)
        else if key > k then member (key, right)
        else true
end