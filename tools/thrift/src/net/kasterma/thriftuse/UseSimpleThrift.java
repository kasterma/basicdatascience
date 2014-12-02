package net.kasterma.thriftuse;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;

import net.kasterma.thriftex1.DataUnit;
import net.kasterma.thriftex1.LabeledNumber;
import net.kasterma.thriftex1.LabeledState;

public class UseSimpleThrift {

	public static void main(String[] args) {
		DataUnit dun = new DataUnit();
		dun.setNum(new LabeledNumber("some_num", (short) 3));

		DataUnit dul = DataUnit.state(new LabeledState("state1", true));
		
		FileOutputStream fos;
		try {
			fos = new FileOutputStream("t.tmp");
			ObjectOutputStream oos = new ObjectOutputStream(fos);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		
		System.out.println(dul.toString());
		System.out.println(dun.toString());
		
		System.out.println("done");

	}

}
