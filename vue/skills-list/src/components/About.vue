<template>
  <div class="about">
    <h1>Hello {{ their_name }}, This is an about page</h1>
    <p>
      Lorem ipsum, dolor sit amet consectetur adipisicing elit. At nemo et
      excepturi ea omnis dicta modi iure odio voluptate illo quaerat quos quas,
      velit perferendis nisi nihil explicabo ad enim?
    </p>
  </div>
</template>

<script>
export default {
  data() {
    return {
      their_name: this.$route.params.name
    };
  }
};
</script>
