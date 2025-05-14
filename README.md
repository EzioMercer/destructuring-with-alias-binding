# Naming destructed variables

---

This proposal suggests a new syntax which will be useful shorthand for saving destructed variables in object
immediately without the need to repeat them to create a new object

In my practice, it is ubiquitous to write something like this:

```js
function foo(config) {
	const _config = {
		...config,
		x: Number(config.x)
	}
	
	console.log(_config);
}
```

And here is the problem for me because `config` can be an object with extra keys which I don't want to have inside the
function.
So I have to rewrite it like this:

```js
function foo({ prop1, prop2, ..., propN, x }) {
	const _config = {
		prop1,
		prop2,
		...,
		propN,
		x: Number(x)
	}
	
	console.log(_config);
}
```

And now the problem is that I have to write the exact same things twice.
So my proposal is to let rewrite the above code in that way:

```js
function foo({ prop1, prop2, ..., propN, x } as config) {
	const _config = {
		...config,
		x: Number(x)
	}
	
	console.log(_config);
}
```

In that way I can access the `x` explicitly writing it or get it from `config` like this `config.x` or `config['x']`

You can browse the [ecmarkup output](https://EzioMercer.github.io/naming-destructed-variables/)
or browse the [source](https://github.com/EzioMercer/naming-destructed-variables/blob/main/spec.emu)
