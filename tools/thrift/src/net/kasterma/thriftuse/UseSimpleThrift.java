package net.kasterma.thriftuse;

import net.kasterma.thriftex1.DataUnit;
import net.kasterma.thriftex1.LabeledNumber;
import net.kasterma.thriftex1.LabeledState;

public class UseSimpleThrift {

	public static void main(String[] args) {
		DataUnit dun = new DataUnit();
		dun.setNum(new LabeledNumber("some_num", (short) 3));

		DataUnit dul = DataUnit.state(new LabeledState("state1", true));
		
		System.out.println(dul.toString());
		System.out.println(dun.toString());
		
		System.out.println("done");

	}

}
