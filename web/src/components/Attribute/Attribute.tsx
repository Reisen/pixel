import React       from 'react';
import styles      from './Attribute.module.css';

interface Props {
    icon: string;
    name: string;
    value: string;
}

export default (props: Props) =>
    <div className={styles.Root}>
        <span>
            <i className={`icofont-${props.icon}`} />
            {props.name}
        </span>

        <span>
            {props.value}
        </span>
    </div>;
