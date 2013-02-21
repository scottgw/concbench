note
	description: "Summary description for {SHARED_QUEUE}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	SHARED_QUEUE

create
	make

feature
	make
		do
			create q.make (10)
			create mutex.make
			create cv.make
		end

	enqueue (i: INTEGER)
		do
			mutex.lock

			q.extend (i)

			if q.count >= 1 then
				cv.broadcast
			end

			mutex.unlock
		end


	dequeue: INTEGER
		do
			mutex.lock

			from
			until not q.is_empty
			loop
				cv.wait (mutex)
			end

			Result := q.item
			q.remove

			mutex.unlock
		end

feature {NONE}
	mutex: MUTEX
	cv: CONDITION_VARIABLE
	q: ARRAYED_QUEUE [INTEGER]

end
