note
	description : "prodcons application root class"
	date        : "$Date$"
	revision    : "$Revision$"

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	make
		local
			ts: ARRAYED_LIST[THREAD]
			p: PRODUCER
			c: CONSUMER
			s: SHARED_QUEUE
			i: INTEGER
		do
			create s.make
			create ts.make (10)
			from i := 1
			until i > 32
			loop
				create p.make_s (s)
				create c.make_s (s)

				ts.extend (p)
				ts.extend (c)

				p.launch
				c.launch

				i := i + 1
			end

			across
				ts as tc
			loop
				tc.item.join
				print ("joined")
			end
		end

end
