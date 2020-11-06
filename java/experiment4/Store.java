public interface Store {
  public String create(Student[] stu, Student createStu, int cnt);

  public String findAll();

  public String findByStudentId(Student[] stu, String idToFind, int cnt);

  public String findByStudentName(Student[] stu, String nameToFind, int cnt);

  public String update(Student[] stu, Student updateStu, int cnt);

  public String delete(Student[] stu, Student deleteStu, int cnt);
}
