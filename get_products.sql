SEL a.product_number, b.product_image_url, c.online_product_type_desc as category, SUM(a.orderline_quantity) AS items, SUM(a.orderline_total_value) AS demand, COUNT(*) AS volume
FROM orderline_current a
JOIN product_summary b
ON a.product_number = b.product_number
LEFT JOIN online_product_type c
ON a.product_number = c.product_number
WHERE a.orderline_status_code NOT IN ('1','3','8','9')
AND a.date_of_accepted_demand BETWEEN DATE '2017-01-01' AND DATE-1
AND b.merch_fmb_code = 1
AND a.financial_group_code = 1
AND c.online_product_type_desc IN ('Dresses','Womens Blouses','Womens Jersey Tops','Womens Trousers','Womens Jackets and Blazers','Skirts','Womens Jumpers','Womens Tunics','Womens Jeans','Womens Cardigans','Womens Coats','Womens T-Shirts','Womens Vests','Womens Jumpsuits','Womens Shirts','Womens Shorts','Kimonos','Leggings')
AND b.product_image_url IS NOT NULL
GROUP BY 1,2,3
ORDER BY 1,2,3