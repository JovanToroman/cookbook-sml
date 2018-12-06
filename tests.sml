(* Feel free to make your own additional ingredients, recipes and cookbooks. *)

val salt = Cookbook.makeIngredient "salt"
val sugar = Cookbook.makeIngredient "sugar"
val honey = Cookbook.makeIngredient "honey"
val natreen = Cookbook.makeIngredient "natreen"
val chicken = Cookbook.makeIngredient "chicken"
val pork = Cookbook.makeIngredient "pork"
val beef = Cookbook.makeIngredient "beef"
val veal = Cookbook.makeIngredient "veal"
val salad = Cookbook.makeIngredient "salad"
val egg = Cookbook.makeIngredient "egg"
val redPepper = Cookbook.makeIngredient "red pepper"
val greenPepper = Cookbook.makeIngredient "green pepper"
val milk = Cookbook.makeIngredient "milk"
val flour = Cookbook.makeIngredient "flour"
val pancake = Cookbook.makeIngredient "pancake"
val scrambledEggs = Cookbook.makeIngredient "scrambled eggs"
val padThai = Cookbook.makeIngredient "pad thai"

val stock = Cookbook.makeStock [
    (salt, 3),
    (sugar, 3),
    (chicken, 2),
    (salad, 1),
    (egg, 5),
    (milk, 4),
    (flour, 4)
]

val pricelist = Cookbook.makePricelist [
    (salt, 0.5),
    (sugar, 0.3),
    (honey, 1.1),
    (natreen, 0.7),
    (chicken, 2.2),
    (pork, 2.8),
    (beef, 2.9),
    (veal, 3.1),
    (salad, 0.8),
    (egg, 0.4),
    (redPepper, 0.5),
    (greenPepper, 0.45),
    (milk, 0.7),
    (flour, 0.9),
    (pancake, 3.5),
    (scrambledEggs, 1.2),
    (padThai, 6.5)
]

val pancakeRecipe = Cookbook.makeRecipe (pancake, Cookbook.makeStock [
    (salt, 1),
    (sugar, 1),
    (egg, 2),
    (milk, 1)
])

val padThaiRecipe = Cookbook.makeRecipe (padThai, Cookbook.makeStock [
    (salt, 1),
    (chicken, 1),
    (sugar, 1),
    (redPepper, 1),
    (flour, 1)
])

val scrambledEggsRecipe = Cookbook.makeRecipe (scrambledEggs, Cookbook.makeStock [
    (egg, 1)
])

val cookbook = Cookbook.makeCookbook [
    pancakeRecipe,
    scrambledEggsRecipe,
    padThaiRecipe
]

val padThaiVariants = Cookbook.generateVariants padThaiRecipe [
    [sugar, natreen],
    [greenPepper, redPepper],
    [pork, veal, beef, chicken]
]

val cheapestPadThaiVariant = valOf (Cookbook.cheapestRecipe padThaiVariants pricelist)

(* ================== DEBUGGING UTILS ================== *)

fun ingredientsEqual a b = Cookbook.ingredientToString a = Cookbook.ingredientToString b
fun stocksEqual a b = Cookbook.stockToString a = Cookbook.stockToString b
fun recipesEqual a b = Cookbook.recipeToString a = Cookbook.recipeToString b
fun pricelistsEqual a b = Cookbook.pricelistToString a = Cookbook.pricelistToString b
fun cookbooksEqual a b = Cookbook.cookbookToString a = Cookbook.cookbookToString b

(* =============== PRINT DEBUGGING UTILS =============== *)

fun printStock stock = print ((Cookbook.stockToString stock) ^ "\n\n")
fun printPricelist pricelist = print ((Cookbook.pricelistToString pricelist) ^ "\n\n")
fun printRecipe recipe = print ((Cookbook.recipeToString recipe) ^ "\n\n")
fun printCookbook cookbook = print ((Cookbook.cookbookToString cookbook) ^ "\n\n")
fun printPrice price = print ((Real.toString price) ^ "\n\n")
fun printRecipeList recipes = print ((String.concatWith "\n\n" (List.map Cookbook.recipeToString recipes)) ^ "\n\n")

(* ======================= TESTS ======================= *)

(* Here be dragons. Test your code well! *)

;

ingredientsEqual salt salt;
ingredientsEqual salt sugar;
