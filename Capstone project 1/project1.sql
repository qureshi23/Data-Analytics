USE restuarant;




#Customer RFM

select * ,
	case 
	when rfm_cell_string in (111, 112 , 121, 122, 123, 132, 211, 212, 114, 141,312,313) then 'lost_customer'  #lost customers
	when rfm_cell_string in (133, 134, 143, 244, 334, 343,411, 344, 144,442,423) then 'slipping away, cannot lose' # (Big spenders who haven’t purchased lately) slipping away
	when rfm_cell_string in (311, 611, 331,222, 223, 233, 322,511,621,521,612,512,513) then 'new customer'
	when rfm_cell_string in (323, 333,321, 422, 332, 432,624,521,623,622,612,522,542,513,423,423) then 'potential churners'
	when rfm_cell_string in (523, 433,421, 422, 432, 532,531,631,533,632,642) then 'active' #Customers who buy often & recently, but at low price points)
	when rfm_cell_string in (433, 434, 443, 444,644,634,544,534,624,534,633,543,643) then 'loyal Customer'
end rfm_segment
from (
	select *, (concat(rfm_recency,rfm_frequency,rfm_monetary))as rfm_cell_string from(
		select *,
			dense_rank() over (order by Recency desc) rfm_recency,
			ntile(4) over (order by Frequency) rfm_frequency,
			ntile(4) over (order by MonetaryValue) rfm_monetary
		 from(
			 Select
					mb.id,
					 mb.first_name,
					 mb.surname,
					 max(od.date) as last_order_date,
					 (select max(date) from orders) as max_order_date,
					 sum(od.total_order) as MonetaryValue,
					 count(od.id)  as Frequency,
					DATEDIFF((select max(date) from orders),max(od.date)) as Recency
				from orders as od 
				join members as mb	
					on od.member_id=mb.id
				where od.member_id=mb.id
				group by mb.first_name,mb.surname
			)a
	)b
)c
;


#RESTAURANT RFM

#SELECT r.id, r.restaurant_name, rt.restaurant_type, c.city, o.date as recency, count(o.id) over (partition by o.restaurant_id) as frequency ,sum(o.total_order) over (partition by r.restaurant_name) as monetry
SELECT r.id, r.restaurant_name, rt.restaurant_type, 
c.city, o.date as recency, count(o.id)  as frequency ,sum(o.total_order) as monetry
FROM restaurants as r
JOIN orders as o
ON r.id = o.restaurant_id
JOIN restaurant_types as rt
ON r.restaurant_type_id = rt.id
JOIN cities as c
ON r.city_id = c.id
GROUP BY r.restaurant_name, o.date, c.city, rt.restaurant_type
ORDER BY o.date asc;



#CITIES RFM

select
	ci.city,
     od.date as Recency,
    count(od.id) as Frequency,
    sum(od.total_order) as Monetary
from orders as od
join restaurants as rs
	on od.restaurant_id=rs.id
join cities	as ci
	on rs.city_id=city_id
where ci.id=rs.city_id
group by city,od.date;

#Meal & Serve Type RFM

SELECT mt.meal_type as meals, st.serve_type, o.date as renceny, count(o.id)  as frequency, sum(o.total_order)as monetary
FROM meal_types AS mt
JOIN meals m ON mt.id = m.meal_type_id
JOIN order_details od ON m.id = od.meal_id
JOIN orders o ON od.order_id = o.id
JOIN serve_types st ON m.serve_type_id = st.id
GROUP BY mt.meal_type, st.serve_type ,o.date;


-- BUSIEST TIME OF THE WEEK 

#SELECT r.restaurant_name as restaurant_name, o.date as date, DAYNAME(o.date) as day , HOUR(o.hour) as hour, minute(o.hour) as time, count(o.total_order)# over (partition by HOUR(o.hour)) as total_orders#, count(o.id)
SELECT r.restaurant_name as restaurant_name, DAYNAME(o.date) as day , 
HOUR(o.hour) as time, count(o.total_order) as total_orders
FROM orders as o
JOIN restaurants as r
ON o.restaurant_id = r.id
GROUP BY  r.restaurant_name, HOUR(o.hour), DAYNAME(o.date)
having count(o.total_order) > 10
ORDER BY r.restaurant_name, o.hour

;





 