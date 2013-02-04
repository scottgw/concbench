package ch.ethz.se.concbench.anneal;

public class City {
	int id;
	int leftId;
	int rightId;
	int indexInRoute;

	City(int id, int indexInRoute, int leftId, int rightId) {
		this.id = id;
		this.leftId = leftId;
		this.rightId = rightId;
		this.indexInRoute = indexInRoute;
	}

}
