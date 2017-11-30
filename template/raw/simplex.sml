structure simplex = struct
  fun const x _ = x

  fun loop first last f =
  let
    (* n.b.: in OCaml, for loops are inclusive; hence the ``>'' rather than a ``='' here. *)
    fun go i = if i > last then () else
          (f i;
           go (i + 1))
  in
    go first
  end

  fun realTupleMin (a : real, b) (c, d) = if a < c then (a, b) else (c, d)
  fun realTupleMax (a : real, b) (c, d) = if a < c then (c, d) else (a, b)

  fun fold i j f init = if i > j then init else fold (i + 1) j f (f i init)
  fun maxf i j f = fold (i + 1) j (fn k => fn a => realTupleMax (f k) a) (f i)
  fun minf i j f = fold (i + 1) j (fn k => fn a => realTupleMin (f k) a) (f i)

  fun adjust f i a = Array.update (a, i, f (Array.sub (a, i)))

  fun matrixSub i j a = Array.sub (Array.sub (a, i), j)

  fun matrixAdjust f i j a = adjust f j (Array.sub (a, i))

  fun matrixUpdate i j v a = matrixAdjust (const v) i j a

  datatype lpAnswer = Infeasible | Unbounded | Feasible of real * real Array.array

  fun simplex m n a b c =
    (*
      input:
        m = #constraints, n =#variables
        max c dot x s.t. a x <= b
        where a = mxn, b = m vector, c = n vector
      output:
        Infeasible, or Unbounded, or a pair Feasible (z,soln) where z is
        the maximum objective function value, and soln is an n-vector of
        variable values.
      caveats:
        Cycling is possible.  Nothing is done to mitigate loss of
        precision when the number of iterations is large.
    *)
    let
      val a0 = a
      val a = Array.tabulate (m + 1, fn _ => Array.array (n + 1, 0.0))

      (* the variables are numbered 0,...,n+m-1 *)
      val basic = Array.tabulate (m, fn j => n + j)
      val nonBasic = Array.tabulate (n, fn i => i)

      val inf = 1e100
      val eps = 1e~9

      fun pivot r c =
        (* first swap variables non_basic.(c) and basic.(r) *)
        let
          val (nc, br) = (Array.sub (nonBasic, c), Array.sub (basic, r))
        in
          Array.update (nonBasic, c, br);
          Array.update (basic, r, nc);

          matrixUpdate r c (1.0 / matrixSub r c a) a;
          loop 0 n (fn j => if j <> c then matrixUpdate r j (matrixSub r j a * matrixSub r c a) a else ());
          loop 0 m (fn i => if i <> r then (
            loop 0 n (fn j =>
              if j <> c then matrixUpdate i j (matrixSub i j a - matrixSub i c a * matrixSub r j a) a else ()
            );
            matrixUpdate i c (~(matrixSub i c a) * matrixSub r c a) a
          ) else ())
        end

      fun feasible () =
      let
        fun loop' () =
          let
            val (p, row) = minf 0 (m - 1) (fn i => (matrixSub i n a, i))
          in
            if p > ~eps then true else
              let
                val (p, col) = minf 0 (n - 1) (fn j => (matrixSub row j a, j))
              in
                if p > ~eps then false else
                  let
                    val (p, row) = minf row (m - 1) (fn i =>
                      if i = row orelse matrixSub i col a > eps then (matrixSub i n a / matrixSub i col a, i)
                      else (inf, i))
                  in
                    pivot row col;
                    loop' ()
                  end
              end
          end
      in
        loop' ()
      end

    in

      loop 0 (m - 1) (fn i =>
        (matrixUpdate i n (Array.sub (b, i)) a;
         loop 0 (n - 1) (fn j =>
           matrixUpdate i j (matrixSub i j a0) a
         ))
      );

      loop 0 (n - 1) (fn j =>
        matrixUpdate m j (Array.sub (c, j)) a
      );

      if not (feasible ()) then Infeasible else
        let
          fun loop' () =
          let
            val (p, col) = maxf 0 (n - 1) (fn i => (matrixSub m i a, i))
          in
            if p < eps then
              let
                val soln = Array.array (n, 0.0)
              in
                loop 0 (m - 1) (fn j =>
                  if Array.sub (basic, j) < n
                    then Array.update (soln, Array.sub (basic, j), matrixSub j n a)
                    else ()
                );
                Feasible (~(matrixSub m n a), soln)
              end
            else
              let
                  val (p, row) = minf 0 (m - 1) (fn i =>
                    if matrixSub i col a > eps then (matrixSub i n a / matrixSub i col a, i) else (inf, i))
              in
                if p >= inf then Unbounded else (
                  pivot row col;
                  loop' ()
                )
              end
          end
        in
          loop' ()
        end
    end

  fun main _ =
    let
      val a = Array.fromList [Array.fromList [1.0, 3.0, 1.0],
                            Array.fromList [~1.0, 0.0, 3.0],
                            Array.fromList [2.0, ~1.0, 2.0],
                            Array.fromList [2.0, 3.0, ~1.0]]
      val b = Array.fromList [3.0, 2.0, 4.0, 2.0]
      val c = Array.fromList [5.0, 5.0, 3.0]
      val (m, n) = (4, 3)
    in
      case simplex m n a b c of
           Infeasible => print "Infeasible\n"
         | Unbounded => print "Unbounded\n"
         | Feasible (z, x) =>
             (print ("the maximum objective function value is " ^ Real.toString z ^ "\n");
              loop 0 (n - 1) (fn i =>
                print ("x" ^ Int.toString i ^ " = " ^ Real.toString (Array.sub (x, i)) ^ "\n")
              ));
      0
    end
end
