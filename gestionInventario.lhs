-- Agregar un nuevo producto al inventario
addProduct :: [(String, Double, Int)] -> String -> Double -> Int -> [(String, Double, Int)]
addProduct inventory name price quantity = inventory ++ [(name, price, quantity)]

-- Actualizar la cantidad de un producto existente
updateQuantity :: [(String, Double, Int)] -> String -> Int -> [(String, Double, Int)]
updateQuantity [] _ _ = []
updateQuantity ((n, p, q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity

-- Eliminar un producto del inventario
removeProduct :: [(String, Double, Int)] -> String -> [(String, Double, Int)]
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory

-- Resumen del inventario: total de productos y valor total
inventorySummary :: [(String, Double, Int)] -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue = sum [p * fromIntegral q | (_, p, q) <- inventory]

-- Buscar un producto en el inventario
searchProduct :: [(String, Double, Int)] -> String -> Maybe (Double, Int)
searchProduct [] _ = Nothing
searchProduct ((n, p, q):xs) name
    | n == name = Just (p, q)
    | otherwise = searchProduct xs name

-- Aplicar un descuento a todos los productos
applyDiscount :: [(String, Double, Int)] -> Double -> [(String, Double, Int)]
applyDiscount inventory discount = [(n, p * (1 - discount / 100), q) | (n, p, q) <- inventory]

-- Función principal
main :: IO ()
main = do
    let inventory = []  -- Inventario inicial vacío
    -- Agregar productos al inventario
    let inventory1 = addProduct inventory "Manzanas" 0.5 100
    let inventory2 = addProduct inventory1 "Platanos" 0.3 150
    let inventory3 = updateQuantity inventory2 "Manzanas" 120  -- Actualizar cantidad de manzanas
    let inventory4 = removeProduct inventory3 "Platanos"  -- Eliminar plátanos del inventario

    -- Obtener resumen del inventario
    let (totalQty, totalValue) = inventorySummary inventory4

    -- Buscar un producto
    let searchResult = searchProduct inventory4 "Manzanas"

    -- Aplicar un descuento del 10%
    let inventoryWithDiscount = applyDiscount inventory4 10

    -- Mostrar resultados
    putStrLn $ "Inventario Final: " ++ show inventory4
    putStrLn $ "Total de productos en stock: " ++ show totalQty
    putStrLn $ "Valor total del inventario: " ++ show totalValue
    putStrLn $ "Resultado de la búsqueda (Manzanas): " ++ show searchResult
    putStrLn $ "Inventario con descuento: " ++ show inventoryWithDiscount
