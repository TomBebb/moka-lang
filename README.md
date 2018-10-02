---


---

<h1 id="moka-programming-language-reference">Moka Programming Language Reference</h1>
<h2 id="introduction">Introduction</h2>
<p><strong>Moka</strong> is a high-level object-oriented programming language with features and constructs combining the best of both worlds with functional programming and OOP.</p>
<h2 id="statements-and-expressions">Statements and Expressions</h2>
<h3 id="declaration-statements">Declaration statements</h3>
<h4 id="variable-declarations">Variable declarations</h4>
<pre><code>var someVar: int = 32
</code></pre>
<h2 id="type-system">Type System</h2>
<h3 id="types">Types</h3>
<h4 id="primitive-types">Primitive Types</h4>
<p>Types that are defined by the language, rather than as part of some standard library, are called <em>primitive types</em>:</p>
<ul>
<li>The boolean type <code>bool</code> with values <code>true</code> and <code>false</code></li>
<li>The integer types:
<ul>
<li><code>byte</code> is a signed 8-bit integer</li>
<li><code>short</code> is a signed 16-bit integer</li>
<li><code>int</code> is a signed 32-bit integer</li>
<li><code>long</code> is a signed 64-bit integer</li>
</ul>
</li>
</ul>
<h4 id="function-types">Function Types</h4>
<p>Functions types are specified in the format:</p>
<pre><code>() 
</code></pre>
<h4 id="struct-types">Struct Types</h4>
<p>A struct type is a heterogeneous product of other types, that are called its fields.</p>
<p>A struct can be defined with the following syntax:</p>
<pre><code>struct Vector {
    var x = 0f32
    var y = 0f32
    fun 
} 
</code></pre>
<h2 id="memory-layout">Memory Layout</h2>
<p>Struct fields are laid out in their sequence of declaration.</p>
<p>Class fields are laid out in their sequence of declaration. Non-final non-static method references are stored sequentially after these fields. Then the super class (if any) is stored as a field. When class is casted to its super class, the super field is referenced and returned. When a class overrides a supers method, the method is not stored in the class.</p>
<p>Class instances are stored as a pointer to the struct above.</p>
<p>Interface references are laid out with pointers to the methods and fields the interfaces contain.</p>
<h2 id="standard-library">Standard Library</h2>
<h3 id="std.async">std.async</h3>
<p><code>interface Future&lt;T&gt;</code><br>
def</p>
<h3 id="std.sys">std.sys</h3>

