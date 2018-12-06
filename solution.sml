exception NotImplemented
structure Dictionary :> DICTIONARY =
struct
    type (''key, 'value) dict = (''key * 'value) list

    val empty = []

    fun exists dict key = 
        case dict of
            [] => false
            | (key1,value)::t => key1 = key orelse exists t key

    fun isEmpty dict =
        case dict of
            [] => true
            | (key,value)::t => false (* try to check here if non-empty elements have appropriate value apirs (key, value); ask what is definition of empty *)

    fun size dict =
        case dict of
            [] => 0
            | (key,value)::t => 1 + size t

    fun get dict key =
        case dict of
            [] => NONE
            | (key1,value)::t => if key = key1 then SOME (value) else get t key

    fun getOrDefault dict key default =
        case dict of
            [] => default
            | (key,value)::t => if key = key then value else getOrDefault t key default (* not tested*)

    fun set dict key value =
        case dict of
            [] => [(key,value)]
            | (key1,value1)::t => if key1 = key then (key, value)::t
            else (key1,value1)::set t key value

    fun remove dict key =
        case dict of
            [] => dict
            | (key1,value)::t => if key = key1 then t else (key1,value)::remove t key

    fun keys dict =
        case dict of
            [] => []
            | (key,value)::t => key::keys t

    fun values dict =
        case dict of
            [] => []
            | (key,value)::t => value::values t

    fun toList dict =
        case dict of
            [] => []
            | (key,value)::t => (key,value)::t

    fun fromList list =
        case list of
            [] => []
            | (key,value)::t => if not (exists t key) 
            then (key,value)::(fromList t) else fromList t

    fun merge [] [] = []
        | merge ((key,value)::t) [] = (key,value)::t
        | merge [] ((key,value)::t) = (key,value)::t
        | merge ((key1,value1)::t1) ((key2,value2)::t2) = if not (exists ((key1,value1)::t1) key2)
        then (key2,value2)::(merge ((key1,value1)::t1) t2) else 
            if key2 = key1 then (key2,value2)::(merge t1 t2) else merge ((key1,value1)::t1) t2

    fun filter f dict =
        case dict of
            [] => []
            | (key,value)::t => if f (key,value) then (key,value)::(filter f t) else filter f t

    fun map f dict =
        case dict of
            [] => []
            | (key,value)::t => (f (key,value))::(map f t)

end

structure Cookbook :> COOKBOOK =
struct
    type ingredient = string
    type stock = (ingredient, int) Dictionary.dict
    type pricelist = (ingredient, real) Dictionary.dict
    type recipe = ingredient * stock
    type cookbook = (ingredient, stock) Dictionary.dict

    exception NoPriceException

    fun makeIngredient name:string =
        name

    fun makeStock ingredients =
        Dictionary.fromList ingredients

    fun makePricelist pricelist =
        Dictionary.fromList pricelist

    fun makeRecipe ((name:ingredient), (ingrs:stock)) =
        (name,ingrs)

    fun makeCookbook (recipes:recipe list) =
        Dictionary.fromList(recipes)

    fun ingredientToString ingredient:ingredient =
        ingredient:string

    fun insert x [] = [x]
        |   insert (key1:string,value1) ((key2:string,value2)::ys) = 
                if key1 < key2
                    then (key1,value1)::((key2,value2)::ys)
                    else
                         (key2,value2) :: (insert (key1,value1) ys)

    fun insertionsort [] = []
        |   insertionsort ((k1,v1)::xs) = insert (k1,v1) (insertionsort xs)

    fun stockToString (stock:stock) =
        let
            val sorted = insertionsort (Dictionary.toList(stock))
        in
            case sorted of
                (name,quant)::t => if quant > 0 then 
                    name ^ ": " ^ Int.toString(quant) ^ "\n" ^ stockToString (Dictionary.fromList(t))
                                    else stockToString (Dictionary.fromList(t))
                | [] => ""
        end
                
    fun pricelistToString (pricelist:pricelist) =
        let
            val sorted = insertionsort (Dictionary.toList(pricelist))
        in
            case sorted of
                (name,price)::t => name ^ ": " ^ Real.toString(price) ^ "\n" ^ pricelistToString (Dictionary.fromList(t))
                | [] => ""
        end

    fun recipeToString (recipe:recipe) =
        case recipe of
            (name, ingrs) => "=== " ^ name ^ " ===\n" ^ stockToString ingrs

    fun cookbookToString (cookbook:cookbook) =
        let
            val sortedR = insertionsort (Dictionary.toList(cookbook))
        in
            case sortedR of
                recipe::t => recipeToString recipe ^ cookbookToString (Dictionary.fromList(t))
                | [] => ""
        end

    fun hasEnoughIngredients (stock:stock) (recipe:recipe) =
        case recipe of
            (name,ingrs) => 
            let
                val ingrList = Dictionary.toList(ingrs)
                
            in
                case ingrList of
                    (k,v)::t => valOf(Dictionary.get stock k) >= v andalso hasEnoughIngredients stock (name,(Dictionary.fromList(t)))
                    | [] => true
            end
                handle Option => false

    fun cook (recipe:recipe) (stock:stock) =
        case recipe of
            (name,ingredients) => 
            let
                val newStock = (Dictionary.map (fn (ing,quant) => (ing,(valOf(Dictionary.get stock ing) - quant))) ingredients)
            in
                if hasEnoughIngredients stock recipe
                then if Dictionary.exists stock name 
                     then Dictionary.set newStock name (valOf(Dictionary.get stock name) + 1) 
                     else Dictionary.set newStock name 1 
                else stock
            end

    fun priceOfStock (stock:stock) (pricelist:pricelist) =
        case stock of
            stock => 
            let
                fun f ((ing,quant), sum) = 
                    (valOf(Dictionary.get pricelist ing) * Real.fromInt(quant) + sum)
                    handle Option => raise NoPriceException
            in
                List.foldr f 0.0 (Dictionary.toList(stock))
            end

    fun priceOfRecipe (recipe:recipe) (pricelist:pricelist) =
        case recipe of
            (_,ingredients) => 
            let
                fun f ((ing,quant), sum) = 
                    (valOf(Dictionary.get pricelist ing) * Real.fromInt(quant) + sum)
                    handle Option => raise NoPriceException
            in
                List.foldr f 0.0 (Dictionary.toList(ingredients))
            end


    fun missingIngredients (recipe:recipe) (stock:stock) =
        case recipe of
            (name,ingrs) => 
            let
                val ingrList = Dictionary.toList(ingrs)
            in
                case ingrList of
                    (ing,quan)::t => 
                            (if hasEnoughIngredients stock recipe then Dictionary.fromList([]) 
                            else case Dictionary.get stock ing of
                            SOME(x) => if x < quan
                            then Dictionary.fromList((ing,(quan - valOf(Dictionary.get stock ing)))::(Dictionary.toList((missingIngredients (name,Dictionary.fromList(t)) stock))))
                            else missingIngredients (name,Dictionary.fromList(t)) stock
                            | _ => Dictionary.fromList((ing,quan)::(Dictionary.toList((missingIngredients (name,Dictionary.fromList(t)) stock)))))
                    | [] => Dictionary.fromList([]) 
            end

    fun possibleRecipes (cookbook:cookbook) (stock:stock) =
        case cookbook of
            cookbook => 
            let
                val cookList = Dictionary.toList(cookbook)
            in
                case cookList of
                     rec1::t => if null (Dictionary.toList(missingIngredients rec1 stock)) 
                         then Dictionary.fromList(rec1::(Dictionary.toList(possibleRecipes (Dictionary.fromList(t)) stock))) 
                         else possibleRecipes (Dictionary.fromList(t)) stock
                    | [] => Dictionary.fromList([])
            end

    (*helper function for generate variants*)
    fun removeStops stock =
        case stock of
            (ing,quan)::t => if ing = "STOP" then removeStops t else (ing,quan)::removeStops t
            | [] => []
    
    (*helper function for generate variants; gets substitutions for specific ingredient*)        
    fun getSubstitutionsForIngredient ing [] = []
        | getSubstitutionsForIngredient ing (h::t) =
            let
                fun cont ingr (hl::tll) = if hl = ingr then true else cont ingr tll
                    | cont ingr [] = false      
                fun removeList listn ingred =
                    case listn of
                        [] => []
                        | h::t => if h = ingred then t else h::removeList t ingred
            in
                if cont ing h then removeList h ing else getSubstitutionsForIngredient ing t
            end
    
    (*helper function for generate variants; generates all recipes with substitutions for 1 recipe  *)      
    fun generateRecipeWithSubstituted1Ing recipe subs ing =
        case recipe of
            (name, stock) => let
                val quan = valOf(Dictionary.get stock ing)
                val stockWOstop = removeStops (Dictionary.toList stock)
            in
                case subs of
                    [] => []
                    | h::t => ((name ^ h), 
                        (Dictionary.set (Dictionary.remove (Dictionary.fromList stockWOstop) ing) h quan))
                        ::generateRecipeWithSubstituted1Ing (name,(Dictionary.fromList stockWOstop)) t ing
            end

    fun generateVariants recipe substitutions =
        case substitutions of
            [] => makeCookbook [recipe]
            | subs => case recipe of
                (name,ingrs) => 
                let
                    val ingrList = Dictionary.toList(ingrs)
                    val recipeWithStop = case ingrList of
                        (ing,quan)::t => (name, Dictionary.fromList((ing,quan)::t @ [("STOP",0)]))
                        | [] => (name, Dictionary.fromList([("STOP",0)]))
                        
                    fun moveElementToTheEnd (recipee:recipe) =
                        case recipee of
                            (name,ingrns) => 
                            let
                                val recipeeList = Dictionary.toList(ingrns)
                            in
                                case recipeeList of
                                    (ingrd, quantt)::t => (name, Dictionary.fromList(t @ [(ingrd,quantt)]))
                                    | [] => (name, (Dictionary.fromList ([])))
                            end 
                in
                    case ingrList of
                        (ing,quan)::t => 
                    if not (ing = "STOP") 
                    then makeCookbook (generateRecipeWithSubstituted1Ing recipeWithStop (getSubstitutionsForIngredient ing substitutions) ing @
                        (Dictionary.toList(generateVariants (moveElementToTheEnd recipeWithStop) substitutions)))
                    else makeCookbook [(name, Dictionary.fromList(removeStops ((ing,quan)::t)))]
                    | [] => makeCookbook ([(name,Dictionary.fromList([]))])
                end

    fun cheapestRecipe (cookbook:cookbook) (pricelist:pricelist) =
        let
            val cookList = Dictionary.toList cookbook
            fun min (recipe1,recipe2) = if (priceOfRecipe recipe1 pricelist) < (priceOfRecipe recipe2 pricelist) 
                                        then recipe1 
                                        else recipe2
        in
            case cookList of
                [] => NONE
                | r1::t => SOME(List.foldl min r1 t)
        end

end
