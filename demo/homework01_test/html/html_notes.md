## HTML Notes
- HTML paragraph
```HTML
<p>
    <!-- lorem (+number) -->
    <strong>ipsum dolor sit amet consectetur, adipisicing elit.</strong> Tempore, consequatur minima repellendus
    explicabo vero
    consequuntur quo nobis, eligendi alias accusamus suscipit beatae delectus. Numquam eveniet at molestiae
    laboriosam, dicta tempora.
</p>
```
- HTML list
```HTML
<ul>
    <li><em>item</em></li>
    <li>List item</li>
    <li>List item</li>
</ul>
<ol>
    <li>hhh</li>
    <li>233</li>
</ol>
```
- HTML table
```HTML
<table>
    <thead>
        <tr>
            <th>Name</th>
            <th>Email</th>
            <th>Age</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Azure</td>
            <td>xxx@gmail.com</td>
            <td>12</td>
        </tr>
        <tr>
            <td>Azure</td>
            <td>xxx@gmail.com</td>
            <td>12</td>
        </tr>
        <tr>
            <td>Azure</td>
            <td>xxx@gmail.com</td>
            <td>12</td>
        </tr>
    </tbody>
</table>
```
- HTML forms
```HTML
<form action="process.php" method="POST">
    <div>
        <label>First Name</label>
        <input type="text" name="firstName" id="">
    </div>
    <br>
    <div>
        <label>Last Name</label>
        <input type="text" name="lastName" id="">
    </div>
    <br>
    <div>
        <label>Email</label>
        <input type="email" name="email" placeholder="Enter email">
    </div>
    <br>
    <div>
        <label>Message</label>
        <textarea name="message"></textarea>
    </div>
    <br>
    <div>
        <label>Gender</label>
        <select name="gender" id="">
            <option value="male">Male</option>
            <option value="female">Female</option>
            <option value="other">Other</option>
        </select>
    </div>
    <br>
    <div>
        <label>Age:</label>
        <input type="number" name="age" value="30">
        <label>Birthday:</label>
        <input type="date" name="birthday" id="">
    </div>
    <br>
    <input type="submit" name="submit" value="Submit">
</form>
```
- HTML buttons
```HTML
<button>Click me</button>
```
- HTML image
```HTML
<a href="sample.jpg">
    <img src="sample.jpg" alt="..." width="233">
</a>
```
- Semantic tags
```html
<body>
    <header id="main-header">
        <h1>My Website</h1>
    </header>

    <section>
        <article class="post">
            <h3>Blog Post One</h3>
            <small>Posted by rapt on Jan. 14</small>
            <p>Lorem ipsum dolor, sit amet consectetur adipisicing elit. Beatae cumque repudiandae soluta veniam, et
                voluptates. Odio, voluptatibus corporis? Minima iure ab earum soluta aut laborum animi, hic veritatis
                consectetur laboriosam.</p>
            <a href="post.html">Read More</a>
        </article>

        <article class="post">
            <h3>Blog Post Two</h3>
            <small>Posted by rapt on Jan. 14</small>
            <p>Lorem ipsum dolor, sit amet consectetur adipisicing elit. Beatae cumque repudiandae soluta veniam, et
                voluptates. Odio, voluptatibus corporis? Minima iure ab earum soluta aut laborum animi, hic veritatis
                consectetur laboriosam.</p>
            <a href="post.html">Read More</a>
        </article>

        <article class="post">
            <h3>Blog Post Three</h3>
            <small>Posted by rapt on Jan. 14</small>
            <p>Lorem ipsum dolor, sit amet consectetur adipisicing elit. Beatae cumque repudiandae soluta veniam, et
                voluptates. Odio, voluptatibus corporis? Minima iure ab earum soluta aut laborum animi, hic veritatis
                consectetur laboriosam.</p>
            <a href="post.html">Read More</a>
        </article>

    </section>

    <aside>
        <h3>Categories</h3>
        <nav>
            <ul>
                <li><a href="#">Category 1</a></li>
                <li><a href="#">Category 2</a></li>
                <li><a href="#">Category 3</a></li>
            </ul>
        </nav>
    </aside>

    <footer id="main-footer">
        <p>Copyright &copy; 2020, My Website</p>
    </footer>

</body>
```
![](note.png)
- Other tags
```html
<blockquote cite="http://sample.com">
    Lorem ipsum dolor sit amet consectetur adipisicing elit. Error aperiam ut aliquam at dolorem. Amet, deserunt!
    Molestiae ipsum quisquam nulla harum, quos odio vero? Culpa vel optio distinctio commodi libero?
</blockquote>

<p>The <abbr title="World Wide Web">WWW</abbr> is awesome</p>
<p><cite>HTML Crash Course</cite> by someone</p>
```