import org.junit.*;
import org.junit.runners.MethodSorters;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.Scanner;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class UnitTest {
  private Student stuTest = null;
  private List listTest = null;
  private String result;
  static int cntTest = 0;
  static Student[] stu = new Student[100];

  @Before
  public void init() throws FileNotFoundException {
    stuTest = new Student();
    listTest = new List();
    transferIntoArray();
  }

  @Test
  public void test01() {
    stuTest.setNo(cntTest + 1);
    stuTest.setName("Coffee");
    stuTest.setGender("Male");
    stuTest.setStudentId("22222222");
    stuTest.setMobilePhoneNumber("1008610");
    stuTest.setMemo("w");
    result = listTest.createInfo(stu, stuTest, cntTest);
    System.out.println(result);
    Assert.assertEquals("Create: success", result);
    cntTest++;
  }

  @Test
  public void test02() {
    stuTest.setNo(cntTest + 1);
    stuTest.setName("Jawa");
    stuTest.setGender("Male");
    stuTest.setStudentId("22222222");
    stuTest.setMobilePhoneNumber("11012011");
    stuTest.setMemo("w");
    result = listTest.createInfo(stu, stuTest, cntTest);
    System.out.println(result);
    Assert.assertEquals("Student Id already exists!", result);
  }

  @Test
  public void test03() {
    stuTest.setNo(cntTest + 1);
    stuTest.setName("Doggy");
    stuTest.setGender("Male");
    stuTest.setStudentId("160410306");
    stuTest.setMobilePhoneNumber("1008610");
    stuTest.setMemo("w");
    result = listTest.createInfo(stu, stuTest, cntTest);
    System.out.println(result);
    Assert.assertEquals("Phone number already exists!", result);
  }

  @Test
  public void test04() {
    listTest.findAllInfo();
  }

  @Test
  public void test05() {
    stuTest.setStudentId("22222222");
    result = listTest.findByStudentIdInfo(stu, stuTest.getStudentId(), cntTest);
    System.out.println(result);
    Assert.assertEquals("success", result);
  }

  @Test
  public void test06() {
    stuTest.setName("Coffee");
    result = listTest.findByStudentNameInfo(stu, stuTest.getName(), cntTest);
    System.out.println(result);
    Assert.assertEquals("success", result);
  }

  @Test
  public void test07() {
    stuTest.setName("Coffee");
    stuTest.setGender("Male");
    stuTest.setStudentId("22222222");
    stuTest.setMobilePhoneNumber("13562659875");
    stuTest.setMemo("T7");
    result = listTest.updateInfo(stu, stuTest, cntTest);
    System.out.println(result);
    Assert.assertEquals("Update: success", result);
  }

  @Test
  public void test08() {
    stuTest.setStudentId("22222222");
    result = listTest.deleteInfo(stu, stuTest, cntTest);
    System.out.println(result);
    Assert.assertEquals("Delete: success", result);
  }

  @After
  public void close() {
    System.out.println("Done");
  }

  public static void transferIntoArray() throws FileNotFoundException {
    int i = 0;
    BufferedReader br = new BufferedReader(new FileReader("/Users/raptazure/experiment4/src/student.txt"));
    try {
      String line = br.readLine();
      for (i = 0; i < 100; i++) {
        stu[i] = new Student();
      }
      i = 0;
      while ((line = br.readLine()) != null) {
        Scanner scanner = new Scanner(line);
        stu[i].setNo(scanner.nextInt());
        stu[i].setName(scanner.next());
        stu[i].setGender(scanner.next());
        stu[i].setStudentId(scanner.next());
        stu[i].setMobilePhoneNumber(scanner.next());
        try {
          stu[i].setMemo(scanner.next());
        } catch (NoSuchElementException e) {
          stu[i].setMemo(" ");
        }
        scanner.close();
        i++;
      }
      cntTest = i;
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        br.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
}
