note
	description: "Summary description for {PRODUCER}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	PRODUCER

inherit
	THREAD

create
	make_s

feature
	make_s (s: SHARED_QUEUE)
		do
			make
			q := s
		end

	q: SHARED_QUEUE
	max: INTEGER = 20000

	execute
		local
			i: INTEGER
		do
			from i := 1
			until i > max
			loop
				q.enqueue (i)
				i := i + 1
			end
		end

end
