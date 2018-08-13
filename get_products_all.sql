SEL b.product_number, b.product_image_url, trim(b.merch_fmb_desc) as merch_fmb_desc, c.online_product_type_desc as category, COUNT(*) AS volume
FROM product_summary b
LEFT JOIN online_product_type c
ON b.product_number = c.product_number
GROUP BY 1,2,3,4
ORDER BY 1,2,3,4