# AVL-Sets
Adelson-Velsky Landis Tree implemented in Haskell, with no libraries.

AVL trees are self-balancing optimisations of binary search trees (BST) where the difference between heights of left and right subtrees cannot be more than one for all nodes. AVL trees maintain their balance with rotations, checking if rotations are required each time the tree is mutated. AVL trees are an ordered data structure.

The time required is O(log n) for lookup, plus a maximum of O(log n) retracing levels (O(1) on average) on the way back to the root, so the operation can be completed in O(log n) time. Rotations are O(1) time. AVL insertions take O(log n) time.

