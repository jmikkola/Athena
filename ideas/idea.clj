(mod routing
     (deftype PatternName String)

     (data Routes
           (Routes (namesUsed (Set PatternName))
                   (routes (List (Tuple Regex PatternName)))))

     (defn :public :pure router () Router
       (Routes empty (list)))

     (defn :public :pure addRoute ((Routes namesUsed routes) (re Regex) (name PatternName)) Router
       (if (contains? namesUsed name)
         (Left (++ "Duplicat pattern name: " name))
         (Right (add namesUsed name) (add (Tuple re name) routes))))

     (defn :public :pure matchRoute ((Routes namesUsed routes) (url String)) (Maybe String)
       (matchAny routes url))

     (defn :pure matchAny
       ((list) _) (Nothing)
       ((cons (Tuple re name) rest) (url String)) (if (matches? re url)
                                                    (Just name)
                                                    (matchAny rest url))))

(comment I'll be honest. This sucks.
         Let's try something else)

(mod routing
     (deftype PatternName String)

     (data Routes = (Routes
                     (namesUsed : Set PatternName)
                     (routes : List (Tuple Regex PatternName))))

     (router :: Router)
     (router = Routes {} [])

     (addRoute :: Routes Regex PatternName -> Router)
     (addRoute (Routes namesUsed routes) pattern name =
               (if (contains? namesUsed name)
                 (Left "dup pattern name")
                 (Right (Routes (conj name namesUsed) (conj (Tupel pattern name) routes)))))

     (matchRoute :: Routes String -> Maybe String)
     (matchRoute [] _ = Nothing)
     (matchRoute [(pattern,name):rest] url =
                 (if (matches? pattern url)
                   (Just name)
                   (matchRoute rest url))))
