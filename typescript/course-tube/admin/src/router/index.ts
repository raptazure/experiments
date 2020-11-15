import Vue from "vue";
import VueRouter, { RouteConfig } from "vue-router";
import Home from "../views/Home.vue";
import Main from "../views/Main.vue";
// import CourseList from "../views/courses/CourseList.vue";
// import CourseEdit from "../views/courses/CourseEdit.vue";
import ResourceCrud from "../views/ResourceCrud.vue";

Vue.use(VueRouter);

const routes: RouteConfig[] = [
  {
    path: "/",
    component: Main,
    children: [
      { name: "home", path: "/", component: Home },
      {
        name: "courses-crud",
        path: "/:resource/list",
        component: ResourceCrud,
        props: true
      }
      // { name: "courses-list", path: "/courses/list", component: CourseList },
      // {
      //   name: "courses-create",
      //   path: "/courses/create",
      //   component: CourseEdit,
      // },
      // {
      //   name: "courses-edit",
      //   path: "/courses/edit/:id",
      //   component: CourseEdit,
      //   props: true,
      // },
    ]
  }
];

const router = new VueRouter({
  routes
});

export default router;
