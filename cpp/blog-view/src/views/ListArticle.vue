<template>
  <div>
    <el-table :data="articles">
      <el-table-column prop="title" label="标题" width="140"></el-table-column>
      <el-table-column prop="body" label="内容" width="250"></el-table-column>
      <el-table-column fixed="left" label="操作" width="100">
        <template slot-scope="scope">
          <el-button @click="modify(scope.row.id)" type="text" size="small">编辑</el-button>
          <el-button @click="remove(scope.row.id)" type="text" size="small">删除</el-button>
        </template>
      </el-table-column>
    </el-table>
  </div>
</template>

<script>
export default {
  data() {
    return {
      articles: []
    };
  },
  created() {
    this.fetch();
  },
  methods: {
    fetch() {
      this.$http.get("articles").then(res => {
        this.articles = res.data;
      });
    },
    modify(id) {
      console.log(id);

      this.$router.push(`/articles/${id}/edit`);
    },
    remove(id) {
      this.$http.delete(`articles/${id}`).then(() => {
        this.$message({
          message: "删除成功",
          type: "success"
        });
        this.fetch();
      });
    }
  }
};
</script>

<style>
</style>