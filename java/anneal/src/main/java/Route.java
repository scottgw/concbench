package ch.ethz.se.concbench.anneal;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

public class Route {
	Route (int size, Map map) {
		route = new Vector<City>(size);
		this.map = map;
		
		_size = size;
		for (int i = 0; i < size; i++) {
			City c = new City(i, i, i - 1, i + 1);
			
			route.add(c);
		}
	}

	private Vector<City> route;
	private Map map;
	
	private int _size;
	
	double distance () {
		double dist = 0.0;
		City prev = route.get (0);
		
		for (int i = 1; i < route.size(); i++) {
			City here = route.get(i);
			dist += map.dist(prev, here);
			prev = here;
		}
		
		return dist;
	}
	
	int size () {
		return _size;
	}
	
	synchronized City getCity (int i) {
		int leftId = i == 0 ? -1 : route.get(i-1).id;
		int rightId = i == route.size() - 1 ? -1 : route.get(i+1).id;
		
		int id = route.get(i).id;
		
		City c = new City (id, i, leftId, rightId);
		c.id = id;
		c.indexInRoute = i;
		c.leftId = leftId;
		c.rightId = rightId;
		
		return c;
	}
	
	synchronized void swap (City a, City b) {
		int i = a.indexInRoute;
		int j = b.indexInRoute;
		City tmp = route.get (i);
		route.set(i, route.get (j));
		route.set(j, tmp);
	}
	
	boolean isValid () {
		// We use a set to make sure we have the same number of unique
		// numbers in the route as the size of the route (ie, no duplications).
		Set <Integer> cities = new HashSet <Integer> ();
		
		for (City c : route) {
			cities.add(c.id);
		}
		
		return route.size() == cities.size();
	}
}
