(* Fractales *)

#load "graphics.cma" ;;
open Graphics ;;
open_graph "";;

(* montagne *)

let montagne (x,y)(z,t) =
  clear_graph ();
  let drawline a b =
    let (x,y) = a and (z,t) = b in
    begin
      moveto x y;
      lineto z t
    end
  in
  let rec pics (x,y)(z,t) n  = match n with
    | 0 -> drawline (x,y) (z,t)
    | n -> 
      begin
	let alea = Random.int (n*10) in
	pics (x,y) (((x+z)/2),(t+alea)) (n-1);
	pics (((x+z)/2),(t+alea)) (z,t) (n-1)
      end
  in
  pics (x,y) (z,t) (*-->*)6(*<-- densité des pics de la montagne*)
;;

(* dragon *)

let dragon n x y z t = (* dragon de précision n, entre les points
			      de coordonnées x,y et z,t *)
  clear_graph ();
  let rec dragon1 n x y z t =
    if n = 1 then
      begin
        moveto x y;
        lineto z t;
      end
    else
      let u = (x + z + t - y) / 2
      and v = (y + t - z + x) / 2 in
      begin
	Graphics.set_color (Graphics.rgb 125 0 255);
        dragon1 (n-1) x y u v;
	Graphics.set_color (Graphics.rgb 0 195 205);
        dragon1 (n-1) z t u v;
      end
  in
  dragon1 n x y z t
;;

let sierpinski p n = (*AKA Tri-force, p = coordonées type (x,y) // n = taille , 500 conseillé*)
  clear_graph ();
  let drawline a b =
    let (x,y) = a and (z,t) = b in
    begin
      moveto x y;
      lineto z t
    end
  in
  let rec triangle n x y = if (n > 10) then
    let a = (x + (n/2),y) and b = (x + ((3*n)/4),y + (int_of_float
							(sqrt (float_of_int (((3*n)*n)/4))))/2)
    and c = (x + (n/4),y + (int_of_float (sqrt (float_of_int
						  (((3*n)*n)/4))))/2)
    and m = (n/2)
    in
    begin 
      drawline a b;
      drawline b c;
      drawline c a;
      triangle m x y;
      let (u,v) = a in
      triangle m u v;
      let (u,v) = c in
      triangle m u v;
    end
    else ()
  in
  let (x,y) = p in
  drawline p (x+n,y);
  drawline (x+n,y) (x+(n/2),y + (int_of_float (sqrt (float_of_int
						  (((3*n)*n)/4)))));
  drawline p (x+(n/2),y + (int_of_float (sqrt (float_of_int
						  (((3*n)*n)/4)))));
  triangle n x y;;
