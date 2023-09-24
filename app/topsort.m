topsort :: [(*,*)] -> [*]
topsort rel = tsort (carrier rel) rel

tsort c r = [], if c=[]
          = error "inconsistent data for tsort",       if m=[]
          = a : tsort (c--[a]) [(u,v)|(u,v)<-r; u~=a], otherwise
            where
            a = hd m
            m = (c -- ran r)
||remarks on the above
|| - it is an invariant that c contains the carrier of relation r
|| - m is the set of elements of c with no predecessor in r

dom r = mkset [u|(u,v)<-r]  ||domain of a relation
ran r = mkset [v|(u,v)<-r]  ||range of a relation
carrier r = union (dom r) (ran r)
union x y = mkset (x++y)