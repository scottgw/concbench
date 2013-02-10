package ch.ethz.se.concbench.prodcons;

import java.util.LinkedList;
import java.util.Queue;

public class SharedQueue <T> {
	private Queue <T> queue;
	
	SharedQueue () {
		queue = new LinkedList <T> ();
	}
	
	synchronized void enqueue (T elem) {
		queue.add(elem);
		if (queue.size() == 1)
			notifyAll();
	}
	
	synchronized T dequeue () throws InterruptedException {
		while (queue.isEmpty())
			wait();
		return queue.poll();
	}
}
