import type {ReactNode} from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import CodeBlock from '@theme/CodeBlock';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title}
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link
            className="button button--primary button--lg"
            to="/docs/getting-started">
            Get Started →
          </Link>
          <Link
            className="button button--secondary button--lg"
            to="https://github.com/ygrek/sqlgg">
            GitHub
          </Link>
        </div>
      </div>
    </header>
  );
}

const sqlExample = `-- @find_products
WITH recent_sales AS (
  SELECT product_id, SUM(quantity) AS total_sold
  FROM orders
  WHERE created_at >= CURRENT_DATE - INTERVAL 30 DAY
  GROUP BY product_id
)
SELECT p.id, p.name, p.price, p.category,
       COALESCE(rs.total_sold, 0) AS sales
FROM products p
LEFT JOIN recent_sales rs ON rs.product_id = p.id
WHERE p.category IN @categories
  AND @choice { InStock { p.stock > 0 } 
              | LowStock { p.stock <= @threshold } 
              | All { 1 = 1 } }
  AND { p.price >= @min_price }?
ORDER BY sales DESC;`;

const ocamlExample = `val find_products :
  T.connection ->
  categories:T.text list ->
  choice:[< \`All | \`InStock | \`LowStock of T.num ] ->
  min_price:T.Types.Decimal.t option ->
  (id:T.num -> name:T.text -> price:T.Types.Decimal.t ->
   category:T.text option -> sales:T.num -> 'a) ->
  'a list IO.future`;

function FeatureSection() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className={clsx('row', styles.codeRow)}>
          <div className={clsx('col col--6')}>
            <div className={styles.featureCard}>
              <Heading as="h3">📝 Write SQL</Heading>
              <CodeBlock language="sql">{sqlExample}</CodeBlock>
            </div>
          </div>
          <div className={clsx('col col--6')}>
            <div className={styles.featureCard}>
              <Heading as="h3">⚡ Get Type-Safe Code</Heading>
              <CodeBlock language="ocaml">{ocamlExample}</CodeBlock>
            </div>
          </div>
        </div>
        
        <div className={clsx('row', styles.featuresGrid)}>
          <div className={clsx('col col--4')}>
            <div className={styles.feature}>
              <Heading as="h3">🔍 Type Inference</Heading>
              <p>
                Automatically infers parameter and result types from your SQL schema. 
                No manual type annotations needed.
              </p>
            </div>
          </div>
          <div className={clsx('col col--4')}>
            <div className={styles.feature}>
              <Heading as="h3">🛡️ Nullability Tracking</Heading>
              <p>
                Precisely tracks nullable columns through JOINs, subqueries, 
                and aggregate functions.
              </p>
            </div>
          </div>
          <div className={clsx('col col--4')}>
            <div className={styles.feature}>
              <Heading as="h3">🎯 Multiple Targets</Heading>
              <p>
                Generate code for OCaml, C++, C#, and Java. 
                OCaml backend is the most feature-complete.
              </p>
            </div>
          </div>
        </div>

        <div className={clsx('row', styles.featuresGrid)}>
          <div className={clsx('col col--4')}>
            <div className={styles.feature}>
              <Heading as="h3">📊 Rich SQL Support</Heading>
              <p>
                CTEs, window functions, JSON operations, choice expressions, 
                list parameters, and more.
              </p>
            </div>
          </div>
          <div className={clsx('col col--4')}>
            <div className={styles.feature}>
              <Heading as="h3">🔧 Customizable Types</Heading>
              <p>
                Use the traits system to map SQL types to your domain types. 
                Override defaults as needed.
              </p>
            </div>
          </div>
          <div className={clsx('col col--4')}>
            <div className={styles.feature}>
              <Heading as="h3">⚙️ Build Integration</Heading>
              <p>
                Integrates with dune for OCaml projects. 
                Generated code is human-readable.
              </p>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}

export default function Home(): ReactNode {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title="Type-safe SQL code generator"
      description="sqlgg generates type-safe database access code from SQL queries">
      <HomepageHeader />
      <main>
        <FeatureSection />
      </main>
    </Layout>
  );
}
