public class List {
    private Store storage;

    public List() {
        setStorage(new Text());
    }

    public Store getStorage() {
        return this.storage;
    }

    public void setStorage(Store storage) {
        this.storage = storage;
    }

    public String createInfo(Student[] stu, Student createStudent, int cnt) {
        return getStorage().create(stu, createStudent, cnt);
    }

    public String findAllInfo() {
        return getStorage().findAll();
    }

    public String findByStudentIdInfo(Student[] stu, String findStudentId, int cnt) {
        return getStorage().findByStudentId(stu, findStudentId, cnt);
    }

    public String findByStudentNameInfo(Student[] stu, String findStudentName, int cnt) {
        return getStorage().findByStudentName(stu, findStudentName, cnt);
    }

    public String updateInfo(Student[] stu, Student updateStudent, int cnt) {
        return getStorage().update(stu, updateStudent, cnt);
    }

    public String deleteInfo(Student[] stu, Student deleteStudent, int cnt) {
        return getStorage().delete(stu, deleteStudent, cnt);
    }
}
