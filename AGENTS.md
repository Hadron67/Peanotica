# Agents.md

- 使用`wolframscript -script <文件名>`来运行一个`.wl`/`.wls`/`.wlt`文件
- `mathlink/`下面为`Perm.wl`使用的mathlink程序的源码

## 代码规范
- 需要局部变量时使用`With`而不是`Module`。除非这个局部变量必须是新符号，比如需要为他定义除了own value以外的规则。
