package ch.ethz.se.concbench.prodcons;

import java.util.Vector;

public class Main {
	private static class Consumer implements Runnable {
		private SharedQueue<Integer> queue;
		
		public Consumer (SharedQueue<Integer> queue) {
			this.queue = queue;
		}
		
		public void run () {
			for (int i = 0; i < maxIters; i++) {
				try {
					queue.dequeue();
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	private static class Producer implements Runnable {
		private SharedQueue<Integer> queue;
		
		public Producer (SharedQueue<Integer> queue) {
			this.queue = queue;
		}
		
		public void run () {
			for (int i = 0; i < maxIters; i++) {
				queue.enqueue(i);
			}
		}
	}
	
	static int maxIters = 20000;
	
	public static void main (String args[]) {
        int maxIters = Integer.parseInt(args[0]);
		int numEach = Integer.parseInt(args[1]);

		Vector <Thread> threads = new Vector<Thread> ();
		SharedQueue <Integer> queue = new SharedQueue <Integer> (); 
		
		for (int i = 0; i < numEach; i++) {
			Thread t = new Thread (new Consumer (queue));
			t.start();
			threads.add(t);
		}
		
		for (int i = 0; i < numEach; i++) {
			Thread t = new Thread (new Producer (queue));
			t.start();
			threads.add(t);
		}
		
		for (Thread t : threads) {
			try {
				t.join();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}
