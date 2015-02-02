package net.kasterma.thriftuse;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

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
		
		File objFile = new File("test.objs");
		OutputStream os;
		try {
			os = new FileOutputStream(objFile);

			ObjectOutputStream oos = new ObjectOutputStream(os);
			oos.writeObject(dun);
			os.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		InputStream is;
		try {
			is = new FileInputStream(objFile);
			ObjectInputStream ois = new ObjectInputStream(is);
			DataUnit nd = (DataUnit) ois.readObject();
			System.out.println("from file");
			System.out.println(nd.toString());
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		System.out.println("done");

	}

}
