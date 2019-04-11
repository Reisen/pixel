import React       from 'react';
import styles      from './Tag.module.css';
import { string }  from 'prop-types';

interface Props {
    icon:  string;
    name:  string;
    value: string;
    onIcon: Function;
}

const Tag = (props: Props) =>
    <div className={styles.Root}>
        <span>
            <i className={`icofont-${props.icon}`} onClick={() => props.onIcon && props.onIcon()}/>
            {props.name}
        </span>

        <span>
            {props.value}
        </span>
    </div>;

export default Tag;
