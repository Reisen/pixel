import React  from 'react';
import styles from './Tag.module.css';


interface Props {
    tag: string;
    category?: string;
}


export default (props: Props) => (
    <div className={styles.Root}>
        <i className="icofont-tag"></i>
        <a href="#" className={styles[`Category--${props.category}`]}>
            {props.tag}
        </a>
    </div>
);
