import java.io.*;

public class Text implements Store {
  @Override
  public String create(Student[] stu, Student createStu, int cnt) {
    for (int i = 0; i < cnt; i++) {
      if (stu[i].getStudentId().equals(createStu.getStudentId()))
        return "Student Id already exists!";
      if (stu[i].getMobilePhoneNumber().equals(createStu.getMobilePhoneNumber()))
        return "Phone number already exists!";
    }

    // begin to append
    BufferedWriter bw = null;
    stu[cnt].setNo(createStu.getNo());
    stu[cnt].setName(createStu.getName());
    stu[cnt].setGender(createStu.getGender());
    stu[cnt].setStudentId(createStu.getStudentId());
    stu[cnt].setMobilePhoneNumber(createStu.getMobilePhoneNumber());
    stu[cnt].setMemo(createStu.getMemo());
    try {
      bw = new BufferedWriter(
          new OutputStreamWriter(new FileOutputStream("/Users/raptazure/experiment4/src/student.txt", true)));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    try {
      bw.newLine();
      bw.write(
          String.format("%02d", createStu.getNo()) + '\t' + createStu.getName() + '\t' + createStu.getGender() + '\t'
              + createStu.getStudentId() + '\t' + createStu.getMobilePhoneNumber() + '\t' + createStu.getMemo() + '\t');
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        bw.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
    return "Create: success";
  }

  @Override
  public String findAll() {
    BufferedReader br = null;
    try {
      br = new BufferedReader(new FileReader("/Users/raptazure/experiment4/src/student.txt"));
      String line;
      while ((line = br.readLine()) != null)
        System.out.println(line);
    } catch (IOException e) {
    } finally {
      try {
        br.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
    return "Search: done";
  }

  @Override
  public String findByStudentId(Student[] stu, String findStudentId, int cnt) {
    for (int i = 0; i < cnt; i++) {
      if (stu[i].getStudentId().equals(findStudentId)) {
        System.out.print(String.format("%02d", stu[i].getNo()));
        System.out.print('\t' + stu[i].getName());
        System.out.print('\t' + stu[i].getGender());
        System.out.print('\t' + stu[i].getStudentId());
        System.out.print('\t' + stu[i].getMobilePhoneNumber());
        System.out.print('\t' + stu[i].getMemo());
        System.out.print(" ");
        System.out.println('\r');
        return "success";
      }
    }
    return "Student not found :(";
  }

  @Override
  public String findByStudentName(Student[] stu, String findStudentName, int cnt) {
    for (int i = 0; i < cnt; i++) {
      if (stu[i].getName().equals(findStudentName)) {
        System.out.print(String.format("%02d", stu[i].getNo()));
        System.out.print('\t' + stu[i].getName());
        System.out.print('\t' + stu[i].getGender());
        System.out.print('\t' + stu[i].getStudentId());
        System.out.print('\t' + stu[i].getMobilePhoneNumber());
        System.out.print('\t' + stu[i].getMemo());
        System.out.print(" ");
        System.out.println('\r');
        return "success";
      }
    }
    return "Student not found :(";
  }

  public void writeToFile(Student[] stu, int cnt) {
    BufferedReader br = null;
    BufferedWriter bw = null;
    try {
      br = new BufferedReader(new FileReader("/Users/raptazure/experiment4/src/student.txt"));
      String line = br.readLine();
      bw = new BufferedWriter(
          new OutputStreamWriter(new FileOutputStream("/Users/raptazure/experiment4/src/student.txt", false)));
      bw.write(line);
      for (int i = 0; i < cnt; i++) {
        if (!stu[i].getStudentId().equals(" ")) {
          bw.newLine();
          bw.write(String.format("%02d", stu[i].getNo()) + '\t' + stu[i].getName() + '\t' + stu[i].getGender() + '\t'
              + stu[i].getStudentId() + '\t' + stu[i].getMobilePhoneNumber() + '\t' + stu[i].getMemo() + '\t');
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        br.close();
        bw.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  @Override
  public String update(Student[] stu, Student updateStudent, int cnt) {
    for (int i = 0; i < cnt; i++) {
      if (stu[i].getStudentId().equals(updateStudent.getStudentId())) {
        stu[i].setName(updateStudent.getName());
        stu[i].setGender(updateStudent.getGender());
        stu[i].setStudentId(updateStudent.getStudentId());
        stu[i].setMobilePhoneNumber(updateStudent.getMobilePhoneNumber());
        stu[i].setMemo(updateStudent.getMemo());
        writeToFile(stu, cnt);
        return "Update: success";
      }
    }
    return "Student not found :(";
  }

  @Override
  public String delete(Student[] stu, Student deleteStudent, int cnt) {
    for (int i = 0; i < cnt; i++) {
      if (stu[i].getStudentId().equals(deleteStudent.getStudentId())) {
        stu[i].setName(" ");
        stu[i].setGender(" ");
        stu[i].setStudentId(" ");
        stu[i].setMobilePhoneNumber(" ");
        stu[i].setMemo(" ");
        writeToFile(stu, cnt);
        return "Delete: success";
      }
    }
    return "Student not found :(";
  }

}
