note
	description: "Summary description for {CONSUMER}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	CONSUMER

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
			i, v: INTEGER
		do
			from i := 1
			until i > max
			loop
				v := q.dequeue
				i := i + 1
			end
		end

end
