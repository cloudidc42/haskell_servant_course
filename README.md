# หลักสูตร Haskell, Servant & Yesod ฉบับสมบูรณ์
## จากระดับพื้นฐาน สู่ระดับมืออาชีพ และ ระดับโลก

---

## เกี่ยวกับหลักสูตรนี้

หลักสูตรนี้ออกแบบมาเพื่อพาคุณเดินทางจากผู้เริ่มต้นที่ไม่รู้จัก Haskell เลย ไปสู่ระดับมืออาชีพที่สามารถพัฒนาแอพพลิเคชันระดับ Production ได้จริง โดยครอบคลุม:

- **Haskell Core** - ภาษาโปรแกรมมิ่งเชิงฟังก์ชันที่ทรงพลัง
- **Servant** - Framework สำหรับสร้าง REST API แบบ Type-safe
- **Yesod** - Framework สำหรับพัฒนา Web Application แบบเต็มรูปแบบ

---

## โครงสร้างหลักสูตร

### ส่วนที่ 1: Haskell พื้นฐาน (Part 01-20)

| Part | หัวข้อ | ระดับ |
|------|--------|-------|
| [01](course/part-01.md) | การติดตั้ง Haskell และ GHC | เริ่มต้น |
| [02](course/part-02.md) | Types พื้นฐานและ Expressions | เริ่มต้น |
| [03](course/part-03.md) | Functions และ Lambda | เริ่มต้น |
| [04](course/part-04.md) | Pattern Matching และ Guards | เริ่มต้น |
| [05](course/part-05.md) | Lists และ Tuples | เริ่มต้น |
| [06](course/part-06.md) | Higher-Order Functions | ปานกลาง |
| [07](course/part-07.md) | Type Classes | ปานกลาง |
| [08](course/part-08.md) | Data Types และ Records | ปานกลาง |
| [09](course/part-09.md) | Maybe และ Either | ปานกลาง |
| [10](course/part-10.md) | IO Monad พื้นฐาน | ปานกลาง |
| [11](course/part-11.md) | Functors และ Applicatives | ปานกลาง |
| [12](course/part-12.md) | Monads เชิงลึก | ขั้นสูง |
| [13](course/part-13.md) | Monad Transformers | ขั้นสูง |
| [14](course/part-14.md) | Lazy Evaluation | ขั้นสูง |
| [15](course/part-15.md) | Type System เชิงลึก | ขั้นสูง |
| [16](course/part-16.md) | GADTs และ Type Families | ขั้นสูง |
| [17](course/part-17.md) | Lenses และ Optics | ขั้นสูง |
| [18](course/part-18.md) | Concurrency และ STM | ขั้นสูง |
| [19](course/part-19.md) | Exceptions และ Error Handling | ขั้นสูง |
| [20](course/part-20.md) | Profiling และ Performance | ขั้นสูง |

### ส่วนที่ 2: Servant Framework (Part 21-50)

| Part | หัวข้อ | ระดับ |
|------|--------|-------|
| [21](course/part-21.md) | Servant แนะนำ และ Setup | ปานกลาง |
| [22](course/part-22.md) | Servant API Types | ปานกลาง |
| [23](course/part-23.md) | Route Handlers | ปานกลาง |
| [24](course/part-24.md) | JSON Serialization | ปานกลาง |
| [25](course/part-25.md) | Path Parameters และ Query Strings | ปานกลาง |
| [26](course/part-26.md) | Request Body และ Headers | ปานกลาง |
| [27](course/part-27.md) | Authentication พื้นฐาน | ขั้นสูง |
| [28](course/part-28.md) | JWT Authentication | ขั้นสูง |
| [29](course/part-29.md) | Database Integration กับ Persistent | ขั้นสูง |
| [30](course/part-30.md) | Database Queries เชิงลึก | ขั้นสูง |
| [31](course/part-31.md) | Middleware และ WAI | ขั้นสูง |
| [32](course/part-32.md) | Error Handling ใน Servant | ขั้นสูง |
| [33](course/part-33.md) | Testing Servant APIs | ขั้นสูง |
| [34](course/part-34.md) | API Documentation ด้วย Swagger | ขั้นสูง |
| [35](course/part-35.md) | Servant Client | ขั้นสูง |
| [36](course/part-36.md) | Streaming Responses | ขั้นสูง |
| [37](course/part-37.md) | WebSockets ใน Servant | ขั้นสูง |
| [38](course/part-38.md) | Rate Limiting และ Caching | ขั้นสูง |
| [39](course/part-39.md) | Microservices Pattern | มืออาชีพ |
| [40](course/part-40.md) | Servant Production Setup | มืออาชีพ |
| [41](course/part-41.md) | Advanced Type-Level Programming | มืออาชีพ |
| [42](course/part-42.md) | Custom Content Types | มืออาชีพ |
| [43](course/part-43.md) | File Upload และ Download | มืออาชีพ |
| [44](course/part-44.md) | GraphQL ด้วย Servant | มืออาชีพ |
| [45](course/part-45.md) | gRPC และ Servant | มืออาชีพ |
| [46](course/part-46.md) | Event Sourcing | มืออาชีพ |
| [47](course/part-47.md) | CQRS Pattern | มืออาชีพ |
| [48](course/part-48.md) | Domain-Driven Design | มืออาชีพ |
| [49](course/part-49.md) | Performance Optimization | มืออาชีพ |
| [50](course/part-50.md) | Deployment และ DevOps | มืออาชีพ |

### ส่วนที่ 3: Yesod Framework (Part 51-80)

| Part | หัวข้อ | ระดับ |
|------|--------|-------|
| [51](course/part-51.md) | Yesod แนะนำ และ Setup | ปานกลาง |
| [52](course/part-52.md) | Routes และ Handlers | ปานกลาง |
| [53](course/part-53.md) | Templates และ Hamlet | ปานกลาง |
| [54](course/part-54.md) | Forms ใน Yesod | ปานกลาง |
| [55](course/part-55.md) | Database ด้วย Persistent | ปานกลาง |
| [56](course/part-56.md) | Authentication ใน Yesod | ขั้นสูง |
| [57](course/part-57.md) | Authorization | ขั้นสูง |
| [58](course/part-58.md) | Sessions และ Cookies | ขั้นสูง |
| [59](course/part-59.md) | Static Files | ขั้นสูง |
| [60](course/part-60.md) | Internationalization | ขั้นสูง |
| [61](course/part-61.md) | Email Integration | ขั้นสูง |
| [62](course/part-62.md) | File Uploads | ขั้นสูง |
| [63](course/part-63.md) | REST API ด้วย Yesod | ขั้นสูง |
| [64](course/part-64.md) | Testing Yesod | ขั้นสูง |
| [65](course/part-65.md) | Subsites | ขั้นสูง |
| [66](course/part-66.md) | Custom Widgets | ขั้นสูง |
| [67](course/part-67.md) | JavaScript Integration | ขั้นสูง |
| [68](course/part-68.md) | WebSockets ใน Yesod | มืออาชีพ |
| [69](course/part-69.md) | Performance Tuning | มืออาชีพ |
| [70](course/part-70.md) | Production Deployment | มืออาชีพ |
| [71](course/part-71.md) | Security Best Practices | มืออาชีพ |
| [72](course/part-72.md) | Caching Strategies | มืออาชีพ |
| [73](course/part-73.md) | Database Migrations | มืออาชีพ |
| [74](course/part-74.md) | Multi-tenant Applications | มืออาชีพ |
| [75](course/part-75.md) | Real-time Features | มืออาชีพ |
| [76](course/part-76.md) | Admin Interface | มืออาชีพ |
| [77](course/part-77.md) | API Rate Limiting | มืออาชีพ |
| [78](course/part-78.md) | Monitoring และ Logging | มืออาชีพ |
| [79](course/part-79.md) | Docker และ Kubernetes | มืออาชีพ |
| [80](course/part-80.md) | CI/CD Pipeline | มืออาชีพ |

### ส่วนที่ 4: ระดับโลก - Advanced Topics (Part 81-100)

| Part | หัวข้อ | ระดับ |
|------|--------|-------|
| [81](course/part-81.md) | Type-Level Programming ขั้นสูง | โลก |
| [82](course/part-82.md) | Dependent Types | โลก |
| [83](course/part-83.md) | Category Theory | โลก |
| [84](course/part-84.md) | Free Monads | โลก |
| [85](course/part-85.md) | Effect Systems | โลก |
| [86](course/part-86.md) | Polysemy | โลก |
| [87](course/part-87.md) | Template Haskell | โลก |
| [88](course/part-88.md) | GHC Plugins | โลก |
| [89](course/part-89.md) | Foreign Function Interface | โลก |
| [90](course/part-90.md) | Compilers ด้วย Haskell | โลก |
| [91](course/part-91.md) | Distributed Systems | โลก |
| [92](course/part-92.md) | Machine Learning ด้วย Haskell | โลก |
| [93](course/part-93.md) | Blockchain Applications | โลก |
| [94](course/part-94.md) | Property-Based Testing | โลก |
| [95](course/part-95.md) | Formal Verification | โลก |
| [96](course/part-96.md) | High-Performance Computing | โลก |
| [97](course/part-97.md) | Open Source Contribution | โลก |
| [98](course/part-98.md) | Advanced Architecture Patterns | โลก |
| [99](course/part-99.md) | System Design | โลก |
| [100](course/part-100.md) | Building World-Class Applications | โลก |

---

## วิธีใช้หลักสูตรนี้

1. **เริ่มจาก Part 01** - ถ้าคุณยังไม่เคยเขียน Haskell
2. **ข้ามไป Part 21** - ถ้าคุณรู้ Haskell พื้นฐานแล้วและต้องการเรียน Servant
3. **ข้ามไป Part 51** - ถ้าคุณต้องการเรียน Yesod โดยตรง

## สิ่งที่ต้องการ

- ความรู้ Programming พื้นฐาน (ภาษาอะไรก็ได้)
- คอมพิวเตอร์ที่ใช้งานได้
- ความมุ่งมั่นในการเรียนรู้!

---

*อัพเดทล่าสุด: 2024*
