note
	description : "condition application root class"
	date        : "$Date$"
	revision    : "$Revision$"

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization
	workers: INTEGER = 32

	make
		local
			i: INTEGER
			var: separate VAR
			worker: separate COND_WORKER
			prod_workers, cons_workers: LINKED_LIST [separate COND_WORKER]
		do
			create var
			create prod_workers.make
			create cons_workers.make

			-- Make equal numbers of "producers" and "consumers, storing each
			-- in a separate list. Depending on what they are intended for,
			-- they are asynchronously run using different procedure calls.
			from
				i := 1
			until
				i > workers
			loop
				create worker.make (var)
				prod_workers.extend (worker)
				run_prod (worker)

				create worker.make (var)
				cons_workers.extend (worker)
				run_cons (worker)

				i := i + 1
			end

			-- Wait for them all to finish.
			from
				i := 1
			until
				i > workers
			loop
				wait (prod_workers [i])
				wait (cons_workers [i])
				i := i + 1
			end
		end

	run_prod (worker: separate COND_WORKER)
		do
			worker.produce
		end

	run_cons (worker: separate COND_WORKER)
		do
			worker.consume
		end

	wait (worker: separate COND_WORKER)
		require
			worker.default = worker.default
		do
		end
end
